package debate

import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManagerFactory
import org.http4s.server.blaze.BlazeServerBuilder
import java.io.InputStream
import java.security.KeyStore
import java.security.SecureRandom

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import scala.concurrent.duration._

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import _root_.org.http4s.server.middleware.HttpsRedirect
import fs2._
import fs2.concurrent.Topic
import org.http4s._
import org.http4s.client.JavaNetClientBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.websocket.WebSocketBuilder

import jjm.DotKleisli
import jjm.implicits._
import jjm.io.FileUtil
import jjm.io.HttpUtil

import debate.quality._
import debate.singleturn.SingleTurnDebateUtils
import debate.singleturn.SingleTurnDebateQuestion
import _root_.io.circe.Json
import org.http4s.client.Client
import scala.concurrent.ExecutionContext

object AnalyticsServer {
  case class Handle(port: Int, restart: IO[Unit], shutdown: IO[Unit])
  // def startPythonServer(port: Int)(implicit cs: ContextShift[IO]): Resource[IO, Handle] =
  //   Resource.make[IO, Handle](
  //     IO {
  //       val x = IO.unit.start
  //       import sys.process._
  //       val p = Process("ls")
  //       // p.
  //       Handle(
  //         port = port,
  //         restart = IO.unit, // TODO
  //         shutdown = IO.unit // TODO
  //       )
  //     }
  //   )(_.shutdown)

  // def makeClientOfPythonServer(handle: Handle, httpClient: Client[IO]): AnalyticsService[IO] = {
  def makeClientOfPythonServer(
    port: Int,
    httpClient: Client[IO],
    stateManager: DebateStateManager,
    blocker: Blocker
  )(implicit cs: ContextShift[IO]): AnalyticsService[IO] = {
    import org.http4s.{Request => HttpRequest}
    import org.http4s._

    val baseUri = Uri(
      scheme = Some(Uri.Scheme.http),
      authority = Some(Uri.Authority(port = Some(port)))
    )

    import org.http4s.circe._

    def doRequest(req: AnalyticsService.Request, httpRequest: HttpRequest[IO]): IO[req.Out] =
      httpClient
        .expect[Json](httpRequest)
        .flatMap { responseJson =>
          IO.fromEither(
            AnalyticsService.Request.analyticsServiceRequestDotDecoder(req)(responseJson.hcursor)
          )
        }

    def get(uri: Uri)  = HttpRequest[IO](method = Method.GET, uri = uri)
    def post(uri: Uri) = HttpRequest[IO](method = Method.POST, uri = uri)

    new AnalyticsService[IO] {
      def refresh =
        stateManager.writeCSVs(blocker) >>
          doRequest(AnalyticsService.Request.Refresh, post(baseUri.withPath("/refresh")))
      def getAnalyticsGraphNames = doRequest(
        AnalyticsService.Request.GetAnalyticsGraphNames,
        get(baseUri.withPath("/all_graphs"))
      )
      def getAnalyticsGraph(name: String) = doRequest(
        AnalyticsService.Request.GetAnalyticsGraph(name),
        get(baseUri.withPath(s"/graph/$name"))
      )
    }
  }

}

case class Server(
  dataPath: NIOPath,
  saveDir: NIOPath,
  blocker: Blocker,
  qualityDataset: Map[String, QuALITYStory],
  singleTurnDebateDataset: Map[String, Vector[SingleTurnDebateQuestion]],
  profiles: Ref[IO, Map[String, Profile]],
  ruleConfigs: Ref[IO, Map[String, RuleConfig]],
  presence: Ref[IO, Map[String, Int]],
  pushUpdate: IO[Unit],
  httpClient: Client[IO],
  slackClientOpt: Option[Slack.Service[IO]],
  officialDebates: DebateStateManager,
  practiceDebates: DebateStateManager,
  mainChannel: Topic[IO, Lobby]
) {

  import Server._

  val qualityService = HttpUtil.makeHttpPostServer(
    new DotKleisli[IO, QuALITYService.Request] {
      import QuALITYService.Request
      def apply(req: Request): IO[req.Out] = {
        val res =
          req match {
            case Request.GetIndex =>
              IO(qualityDataset.mapVals(_.title))
            case Request.GetStory(articleId) =>
              IO(qualityDataset(articleId))
          }
        // not sure why it isn't inferring the type...
        res.asInstanceOf[IO[req.Out]]
      }
    }
  )
  val ajaxService = HttpUtil.makeHttpPostServer(
    new DotKleisli[IO, AjaxService.Request] {
      import AjaxService.Request
      def apply(req: Request): IO[req.Out] = {
        val res =
          req match {
            case Request.GetDebaters =>
              profiles.get
          }
        // not sure why it isn't inferring the type...
        res.asInstanceOf[IO[req.Out]]
      }
    }
  )

  def run(
    jsPath: NIOPath,
    jsDepsPath: NIOPath,
    port: Int,
    analyticsPort: Int,
    executionContext: ExecutionContext,
    ssl: Boolean = false
  )(implicit ce: ConcurrentEffect[IO], timer: Timer[IO], cs: ContextShift[IO]): IO[Unit] =
    getBuilder(ssl, executionContext).flatMap { builder =>
      // AnalyticsServer
      //   .startPythonServer(analyticsPort)
      //   .use { analyticsServer =>
      val analyticsClient = AnalyticsServer
        .makeClientOfPythonServer(analyticsPort, httpClient, officialDebates, blocker)
      val app = httpApp(jsPath, jsDepsPath, analyticsClient)
      builder.bindHttp(port, "0.0.0.0").withHttpApp(app).serve.compile.drain
      // }
    }

  def getBuilder(
    ssl: Boolean,
    executionContext: ExecutionContext
  )(implicit ce: ConcurrentEffect[IO], timer: Timer[IO]) =
    if (!ssl)
      IO.pure(BlazeServerBuilder[IO](executionContext))
    else {
      getSslContext.attempt >>= {
        case Right(sslContext) =>
          IO.pure(BlazeServerBuilder[IO](executionContext).withSslContext(sslContext))
        case Left(e) =>
          IO(System.err.println(s"HTTPS Configuration failed: ${e.getMessage}"))
            .as(BlazeServerBuilder[IO](executionContext))
      }
    }

  val getSslContext =
    for {
      password <- IO(
        new java.util.Scanner(getClass.getClassLoader.getResourceAsStream("password"))
          .next
          .toCharArray
      )
      keystore <- IO {
        val keystoreInputStream: InputStream = getClass
          .getClassLoader
          .getResourceAsStream("keystore.jks")
        require(keystoreInputStream != null, "Keystore required!")
        val keystore: KeyStore = KeyStore.getInstance("jks")
        keystore.load(keystoreInputStream, password)
        keystore
      }
      sslContext <- IO {
        val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
        keyManagerFactory.init(keystore, password)

        val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
        tmf.init(keystore)

        val context = SSLContext.getInstance("TLS")
        context.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
        context
      }
    } yield sslContext

  def httpApp(
    jsPath: NIOPath,
    jsDepsPath: NIOPath,
    analyticsClient: AnalyticsService[IO]
  )(implicit timer: Timer[IO], c: Concurrent[IO], cs: ContextShift[IO]): Http[IO, IO] =
    HttpsRedirect(
      Router(
        s"/$qualityServiceApiEndpoint"   -> qualityService,
        s"/$ajaxServiceApiEndpoint"      -> ajaxService,
        s"/$analyticsServiceApiEndpoint" -> HttpUtil.makeHttpPostServer(analyticsClient),
        s"/"                             -> service(jsPath, jsDepsPath)
      )
    ).orNotFound

  import org.http4s.dsl.io._

  object NameParam extends QueryParamDecoderMatcher[String]("name")

  /** Build the HTTP service (i.e., request -> IO[response]) running the
    * webapp's functionality.
    */
  def service(
    jsPath: NIOPath,
    jsDepsPath: NIOPath
  )(implicit timer: Timer[IO], c: Concurrent[IO], cs: ContextShift[IO]) = {

    // Operations executed by the server

    def registerDebater(profile: Profile) =
      for {
        _        <- profiles.update(_ + (profile.name -> profile))
        _        <- pushUpdate
        debaters <- profiles.get
        _        <- FileUtil.writeJson(profilesSavePath(saveDir))(debaters)
      } yield ()

    def registerRuleConfig(ruleConfig: RuleConfig) =
      for {
        _           <- ruleConfigs.update(_ + (ruleConfig.name -> ruleConfig))
        _           <- pushUpdate
        ruleConfigs <- ruleConfigs.get
        _           <- FileUtil.writeJson(ruleConfigsSavePath(saveDir))(ruleConfigs)
      } yield ()

    def removeDebater(name: String) =
      for {
        _        <- profiles.update(_ - name)
        _        <- pushUpdate
        debaters <- profiles.get
        _        <- FileUtil.writeJson(profilesSavePath(saveDir))(debaters)
      } yield ()

    def createLobbyWebsocket(profile: String) =
      for {
        debaters           <- profiles.get
        currentRuleConfigs <- ruleConfigs.get
        officialRooms      <- officialDebates.getRoomMetadata
        practiceRooms      <- practiceDebates.getRoomMetadata
        leaderboard        <- officialDebates.getLeaderboard
        allDebaters = officialRooms.unorderedFoldMap(_.roleAssignments.values.toSet)
        _           <- presence.update(_ |+| Map(profile -> 1))
        _           <- pushUpdate
        curPresence <- presence.get
        outStream = Stream
          .emit[IO, Option[Lobby]](
            Some(
              Lobby(
                debaters,
                allDebaters,
                curPresence.keySet,
                officialRooms,
                practiceRooms,
                leaderboard,
                currentRuleConfigs
              )
            )
          ) // send the current set of debaters and rooms on connect
          .merge(Stream.awakeEvery[IO](30.seconds).map(_ => None)) // send a heartbeat every 30s
          .merge(mainChannel.subscribe(100).map(Some(_))) // and subscribe to the main channel
          .map(pickleToWSFrame(_))
          .through(filterCloseFrames) // I'm not entirely sure why I remove the close frames.
        res <- WebSocketBuilder[IO].build(
          send = outStream,
          receive =
            x =>
              x.through(filterCloseFrames)
                .map(unpickleFromWSFrame[MainChannelRequest])
                .evalMap {
                  case Poke(roomName, pokees) =>
                    profiles
                      .get
                      .flatMap { profs =>
                        slackClientOpt.traverse_(slack =>
                          pokees
                            .toSortedSet
                            .toVector
                            .traverse_(pokee =>
                              slack.sendMessage(
                                profs,
                                pokee,
                                s"You've been poked in room `$roomName`!"
                              )
                            )
                        )
                      }
                  case RegisterDebater(profile) =>
                    registerDebater(profile)
                  case RegisterRuleConfig(ruleConfig) =>
                    registerRuleConfig(ruleConfig)
                  case RemoveDebater(name) =>
                    removeDebater(name)
                  case DeleteRoom(isOfficial, roomName) =>
                    if (isOfficial)
                      officialDebates.deleteDebate(roomName)
                    else
                      practiceDebates.deleteDebate(roomName)
                  case CreateRoom(isOfficial, roomName, setupSpec) =>
                    if (isOfficial)
                      officialDebates.createDebate(roomName, setupSpec)
                    else
                      practiceDebates.createDebate(roomName, setupSpec)
                },
          onClose = presence.update(p => (p |+| Map(profile -> -1)).filter(_._2 > 0)) >> pushUpdate
        )
      } yield res

    // _root_ prefix because fs2 imports "io"
    // import _root_.io.circe.syntax._
    // import org.http4s.circe._ // for json encoder, per https://http4s.org/v0.19/json/

    val jsLocation     = "out.js"
    val jsMapLocation  = jsLocation + ".map"
    val jsDepsLocation = "deps.js"

    HttpRoutes.of[IO] {

      // homepage
      case GET -> Root =>
        Ok(
          debate.Page(jsLocation = jsLocation, jsDepsLocation = jsDepsLocation).render,
          Header("Content-Type", "text/html")
        )

      // download all save data
      case req @ GET -> Root / "download" =>
        val saveZipPath = Paths.get(saveDir.toString + ".zip")
        Utils.zipDirectory(saveZipPath, saveDir, exclude = _.getFileName() == "profiles.json") >>
          StaticFile.fromString(saveZipPath.toString, blocker, Some(req)).getOrElseF(NotFound())

      // js file
      case req @ GET -> Root / `staticFilePrefix` / `jsLocation` =>
        StaticFile.fromString(jsPath.toString, blocker, Some(req)).getOrElseF(NotFound())

      // source map
      case req @ GET -> Root / `staticFilePrefix` / `jsMapLocation` =>
        StaticFile.fromString(jsPath.toString + ".map", blocker, Some(req)).getOrElseF(NotFound())

      // js deps
      case req @ GET -> Root / `staticFilePrefix` / `jsDepsLocation` =>
        StaticFile.fromString(jsDepsPath.toString, blocker, Some(req)).getOrElseF(NotFound())

      // connect to the lobby to see open rooms / who's in them, etc.
      case GET -> Root / "main-ws" :? NameParam(profile) =>
        createLobbyWebsocket(profile)

      // Connect via websocket to the messaging channel for the given debate.
      // The participant is added to the debate state and then removed when the websocket closes.
      case GET -> Root / "official-ws" / roomName :? NameParam(participantName) =>
        officialDebates.createWebsocket(roomName, participantName)
      case GET -> Root / "practice-ws" / roomName :? NameParam(participantName) =>
        practiceDebates.createWebsocket(roomName, participantName)

    }
  }
}
object Server {

  def profilesSavePath(saveDir: NIOPath)    = saveDir.resolve("profiles.json")
  def ruleConfigsSavePath(saveDir: NIOPath) = saveDir.resolve("rules.json")
  // TODO delete this
  def debatersSavePath(saveDir: NIOPath) = saveDir.resolve("debaters.json")
  def practiceRoomsDir(saveDir: NIOPath) = saveDir.resolve("practice")
  def officialRoomsDir(saveDir: NIOPath) = saveDir.resolve("official")

  def tokenizeStory(x: String): Vector[String] = {
    val res = x
      .split("\n")
      .toVector
      .map((x: String) => jjm.corenlp.Tokenizer.tokenize(x).map(_.token))
      .intercalate(Vector("\n"))
      .filter(_.nonEmpty)
    res
  }

  def initializeDebate(
    qualityDataset: Map[String, QuALITYStory]
  )(setupSpec: DebateSetupSpec): IO[DebateSetup] = {
    val sourceMaterialIO =
      setupSpec.sourceMaterial match {
        case CustomSourceMaterialSpec(title, contents) =>
          IO.pure(CustomSourceMaterial(title, tokenizeStory(contents)))
        case QuALITYSourceMaterialSpec(articleId) =>
          IO(qualityDataset(articleId)).map { qualityStory =>
            QuALITYSourceMaterial(
              articleId = articleId,
              title = qualityStory.title,
              contents = tokenizeStory(qualityStory.article)
            )
          }
      }
    sourceMaterialIO.map { sourceMaterial =>
      DebateSetup(
        setupSpec.rules,
        sourceMaterial,
        setupSpec.question,
        setupSpec.answers.filter(_.nonEmpty),
        setupSpec.correctAnswerIndex,
        setupSpec.roles,
        setupSpec.offlineJudges,
        System.currentTimeMillis()
      )
    }
  }

  def create(dataPath: NIOPath, saveDir: NIOPath, blocker: Blocker)(
    implicit cs: ContextShift[IO]
  ) = {

    val httpClient = JavaNetClientBuilder[IO](blocker).create

    // import org.http4s.client.blaze._
    // BlazeClientBuilder[IO](scala.concurrent.ExecutionContext.global)
    //   .resource
    //   .use { httpClient =>

    for {
      singleTurnDebateDataset <- SingleTurnDebateUtils.readSingleTurnDebate(dataPath, blocker)
      qualityDataset          <- QuALITYUtils.readQuALITY(dataPath, blocker)
      qualityMatches = Utils.identifyQualityMatches(qualityDataset, singleTurnDebateDataset)
      profiles <- FileUtil
        .readJson[Map[String, Profile]](profilesSavePath(saveDir))
        .recoverWith { case e: Throwable =>
          IO {
            println(
              "Error reading debaters JSON. Initializing to empty. This may happen when " +
                "loading a new development server for the first time, because we don't want to " +
                "load slack emails, which would lead to poking during testing."
            )
            println(s"--->\tError message: ${e.getMessage()}")
            List[Profile]().view.map(p => p.name -> p).toMap
          }
        }
      ruleConfigs <- FileUtil
        .readJson[Map[String, RuleConfig]](ruleConfigsSavePath(saveDir))
        .recoverWith { case e: Throwable =>
          IO {
            println("Error reading rules JSON. Initializing to empty.")
            println(s"--->\tError message: ${e.getMessage()}")
            Map.empty[String, RuleConfig]
          }
        }
      profilesRef    <- Ref[IO].of(profiles)
      ruleConfigsRef <- Ref[IO].of(ruleConfigs)
      presenceRef    <- Ref[IO].of(Map.empty[String, Int])
      pushUpdateRef  <- Ref[IO].of(IO.unit)
      slackClientOpt <- FileUtil
        .readString(Paths.get("slack-token.txt"))
        .attempt
        .map(_.toOption)
        .map(_.map(token => Slack.Service.fullHttpClient(httpClient, token)))
      officialDebates <- DebateStateManager.init(
        initializeDebate(qualityDataset),
        officialRoomsDir(saveDir),
        profilesRef,
        pushUpdateRef,
        slackClientOpt
      )
      practiceDebates <- DebateStateManager.init(
        initializeDebate(qualityDataset),
        practiceRoomsDir(saveDir),
        profilesRef,
        pushUpdateRef,
        None // don't send slack notifications for practice rooms
      )
      officialRooms <- officialDebates.getRoomMetadata
      practiceRooms <- practiceDebates.getRoomMetadata
      // channel to update all clients on the lobby state
      leaderboard <- officialDebates.getLeaderboard
      allDebaters = officialRooms.unorderedFoldMap(_.roleAssignments.values.toSet)
      _ <- profilesRef
        .get
        .map(_.isEmpty)
        .ifM(
          ifTrue =
            IO(println("Initializing profiles to include all assigned debaters.")) >>
              profilesRef.set(allDebaters.view.map(name => name -> Profile(name, None)).toMap),
          ifFalse = IO.unit
        )
      mainChannel <- Topic[IO, Lobby](
        Lobby(
          profiles,
          allDebaters,
          Set.empty[String],
          officialRooms,
          practiceRooms,
          leaderboard,
          ruleConfigs
        )
      )
      pushUpdate = {
        for {
          profiles      <- profilesRef.get
          ruleConfigs   <- ruleConfigsRef.get
          presence      <- presenceRef.get
          officialRooms <- officialDebates.getRoomMetadata
          practiceRooms <- practiceDebates.getRoomMetadata
          leaderboard   <- officialDebates.getLeaderboard
          allDebaters = officialRooms.unorderedFoldMap(_.roleAssignments.values.toSet)
          _ <- mainChannel.publish1(
            Lobby(
              profiles,
              allDebaters,
              presence.keySet,
              officialRooms,
              practiceRooms,
              leaderboard,
              ruleConfigs
            )
          )
        } yield ()
      }
      _ <- pushUpdateRef.set(pushUpdate)
    } yield Server(
      dataPath = dataPath,
      saveDir = saveDir,
      blocker = blocker,
      qualityDataset = qualityDataset,
      singleTurnDebateDataset = singleTurnDebateDataset,
      profiles = profilesRef,
      ruleConfigs = ruleConfigsRef,
      presence = presenceRef,
      pushUpdate = pushUpdate,
      httpClient = httpClient,
      slackClientOpt = slackClientOpt,
      officialDebates = officialDebates,
      practiceDebates = practiceDebates,
      mainChannel = mainChannel
    )
  }
}
