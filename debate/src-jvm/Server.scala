package debate

import java.io.InputStream
import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths
import java.security.KeyStore
import java.security.SecureRandom

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.data.NonEmptySet
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import _root_.org.http4s.server.middleware.HttpsRedirect
import fs2._
import fs2.concurrent.Topic
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManagerFactory
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.JavaNetClientBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder

import jjm.implicits._
import jjm.io.FileUtil
import jjm.io.HttpUtil

import debate.quality._
import debate.service._
import debate.singleturn.SingleTurnDebateQuestion
import debate.singleturn.SingleTurnDebateUtils
import debate.util.DenseDistribution
import debate.util.SparseDistribution

case class Server(
  dataPath: NIOPath,
  saveDir: NIOPath,
  blocker: Blocker,
  qualityDataset: Map[String, QuALITYStory],
  singleTurnDebateDataset: Map[String, Vector[SingleTurnDebateQuestion]],
  qualityMatches: Map[String, NonEmptySet[String]], // map from story ID to question IDs
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
    new QuALITYService[IO] {
      def getIndex                    = IO(qualityDataset.mapVals(_.title))
      def getStory(articleId: String) = IO(qualityDataset(articleId))
    }
  )
  val ajaxService = HttpUtil.makeHttpPostServer(
    new AjaxService[IO] {
      def getDebaters = profiles.get
      def getSourceMaterialIndex = officialDebates
        .rooms
        .get
        .map { rooms =>
          qualityDataset.map { case (articleId, story) =>
            articleId ->
              QuALITYStoryMetadata(
                articleId = story.articleId,
                title = story.title,
                splits = story.questions.map(_._2.split).toSet,
                source = story.source,
                numSingleTurnDebateMatches = qualityMatches.get(articleId).foldMap(_.size.toInt),
                hasBeenDebated = rooms
                  .values
                  .exists(room =>
                    SourceMaterial
                      .quality
                      .getOption(room.debate.debate.setup.sourceMaterial)
                      .exists(_.articleId == articleId)
                  )
              )
          }
        }

      def getStoryAndMatches(articleId: String): IO[(QuALITYStory, Set[String])] = IO(
        qualityDataset(articleId) -> qualityMatches.get(articleId).foldMap(_.toSortedSet)
      )

      import debate.scheduler._
      def sampleSchedule(
        workloadDist: SparseDistribution[String],
        ruleDist: SparseDistribution[RuleConfig],
        articleId: String,
        questionIds: Set[String],
        numDebatesPerQuestion: Int
      ): IO[Option[Vector[DebateSetup]]] =
        for {
          rooms <- officialDebates.rooms.get
          allDebates = rooms.values.view.map(_.debate.debate).toVector
          complete   = allDebates.filter(_.isOver).map(_.setup)
          incomplete = allDebates.filterNot(_.isOver).map(_.setup)
          story <- IO(qualityDataset(articleId))
          qas = DebateScheduler.getQAsForStory(story)
          rand         <- IO(new scala.util.Random)
          creationTime <- IO(System.currentTimeMillis())
          schedulesOpt =
            DebateScheduler
              .efficientlySampleSchedules(
                desiredWorkload = workloadDist,
                rules = ruleDist,
                complete = complete,
                incomplete = incomplete,
                sourceMaterial = QuALITYSourceMaterial(
                  articleId = story.articleId,
                  title = story.title,
                  contents = tokenizeStory(story.article)
                ),
                qas = qas.filter(qa => questionIds.contains(qa.questionId)),
                numDebatesPerQuestion = numDebatesPerQuestion,
                // debaters = Map(), // people.mapVals(_ => DebaterLoadConstraint(None, None)),
                creationTime = creationTime,
                rand
              )
              .toOption
          scheduleDistOpt = schedulesOpt
            .map(sample => DenseDistribution.fromSoftmax[Schedule](sample, -_.cost))
          schedule <- IO(scheduleDistOpt.map(_.sample(rand)))
        } yield schedule.map(_.novel)

      def sampleOfflineJudging(
        excludes: Set[String],
        maxNumJudgesForOnline: Int,
        maxNumJudgesForOffline: Int
      ): IO[Either[String, DebateScheduler.OfflineJudgeSchedulingResult]] =
        for {
          profiles <- profiles.get
          judges = profiles.keySet -- excludes
          rooms <- officialDebates.rooms.get
          // roomMetadata <- officialDebates.getRoomMetadata
          // storyRecord = RoomMetadata.constructStoryRecord(roomMetadata)
          rand <- IO(new scala.util.Random)
        } yield profiles.keySet.toNes match {
          case None =>
            Left("No possible judges remaining to assign.")
          case Some(peopleNonEmpty) =>
            Right(
              DebateScheduler.sampleOfflineJudges(
                rooms.mapVals(_.debate.debate),
                peopleNonEmpty,
                judges,
                maxNumJudgesForOnline,
                maxNumJudgesForOffline,
                rand
              )
            )
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

    def removeRuleConfig(ruleConfigName: String) =
      for {
        _           <- ruleConfigs.update(_ - ruleConfigName)
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
        leaderboard        <- officialDebates.leaderboard.get
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
                  case RemoveRuleConfig(ruleConfigName) =>
                    removeRuleConfig(ruleConfigName)
                  case RemoveDebater(name) =>
                    removeDebater(name)
                  case RefreshLeaderboard() =>
                    officialDebates.refreshLeaderboard >> officialDebates.pushUpdateRef.get.flatten
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
                  case CreateRooms(isOfficial, setups) =>
                    if (isOfficial)
                      officialDebates.createDebates(setups)
                    else
                      practiceDebates.createDebates(setups)
                  case ScheduleOfflineJudges(isOfficial, assignments) =>
                    if (isOfficial)
                      officialDebates.scheduleOfflineJudges(assignments)
                    else
                      practiceDebates.scheduleOfflineJudges(assignments)
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
        Utils.zipDirectory(
          saveZipPath,
          saveDir,
          exclude = _.getFileName().toString == "profiles.json"
        ) >> StaticFile.fromString(saveZipPath.toString, blocker, Some(req)).getOrElseF(NotFound())

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
              """Could not read debaters JSON. Initializing to empty. This may happen when
                |loading a new development server for the first time, because we don't want to
                |load slack emails, which would lead to poking during testing.""".stripMargin.trim
            )
            println(s"--->\tMessage: ${e.getMessage()}")
            List[Profile]().view.map(p => p.name -> p).toMap
          }
        }
      ruleConfigs <- FileUtil
        .readJson[Map[String, RuleConfig]](ruleConfigsSavePath(saveDir))
        .recoverWith { case e: Throwable =>
          IO {
            println("""Could not read rules JSON. Initializing to empty. This happens when
                      |there are no saved rule configurations, e.g., when loading a new development
                      |server for the first time.""".stripMargin.trim)
            println(s"--->\tMessage: ${e.getMessage()}")
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
      leaderboard <- officialDebates.leaderboard.get
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
          leaderboard   <- officialDebates.leaderboard.get
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
      qualityMatches = qualityMatches,
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
