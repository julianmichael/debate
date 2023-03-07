package debate

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

  def httpApp(
    jsPath: NIOPath,
    jsDepsPath: NIOPath
  )(implicit timer: Timer[IO], c: Concurrent[IO], cs: ContextShift[IO]) =
    HttpsRedirect(
      Router(
        s"/$qualityServiceApiEndpoint" -> qualityService,
        s"/$ajaxServiceApiEndpoint"    -> ajaxService,
        "/"                            -> service(jsPath, jsDepsPath)
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
        .recoverWith { case _: Throwable =>
          FileUtil
            .readJson[Set[String]](debatersSavePath(saveDir))
            .map(_.map(name => name -> Profile(name, None)).toMap)
            .recoverWith { case e: Throwable =>
              IO {
                println("Error reading debaters JSON. Initializing to JSON with a dummy profile.")
                println(s"--->\tError message: ${e.getMessage()}")
                List(Profile("John Doe", None)).view.map(p => p.name -> p).toMap
              }
            }
        }
      ruleConfigs <- FileUtil
        .readJson[Map[String, RuleConfig]](ruleConfigsSavePath(saveDir))
        .recoverWith { case e: Throwable =>
          IO {
            println("Error reading debaters JSON. Initializing to JSON with a dummy profile.")
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
      slackClientOpt = slackClientOpt,
      officialDebates = officialDebates,
      practiceDebates = practiceDebates,
      mainChannel = mainChannel
    )
  }

}