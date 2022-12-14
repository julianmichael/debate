package debate

import java.io.InputStream
import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths
import java.security.KeyStore
import java.security.SecureRandom

import scala.concurrent.duration._

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import _root_.org.http4s.server.middleware.HttpsRedirect
import com.monovore.decline._
import com.monovore.decline.effect._
import fs2._
import fs2.concurrent.Topic
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManagerFactory
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.CORS
import org.http4s.server.middleware.CORSConfig
import org.http4s.server.websocket.WebSocketBuilder

import jjm.DotKleisli
import jjm.implicits._
import jjm.io.FileUtil
import jjm.io.HttpUtil

import debate.quality._

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object Serve
    extends CommandIOApp(name = "mill -i debate.jvm.run", header = "Serve the live chat app.") {

  val portO = Opts
    .option[Int]("port", metavar = "<port number>", help = "Port where to host the server.")
    .withDefault(8080)

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "<directory path>",
      help = "Directory in which to save the debates."
    )
    .withDefault(Paths.get("save"))

  val sslO = Opts.flag("ssl", help = "Whether to use SSL encryption/host over HTTPS.").orFalse

  def getBuilder(ssl: Boolean) =
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

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = (portO, saveO, sslO).mapN { (port, save, ssl) =>
    for {
      builder <- getBuilder(ssl)
      _ <- Blocker[IO].use { blocker =>
        makeHttpApp(save, blocker)
          .flatMap(app => builder.bindHttp(port, "0.0.0.0").withHttpApp(app).serve.compile.drain)
      }
    } yield ExitCode.Success
  }

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
        System.currentTimeMillis()
      )
    }
  }

  val qualityDataPath                    = Paths.get("data")
  def debatersSavePath(saveDir: NIOPath) = saveDir.resolve("debaters.json")
  def practiceRoomsDir(saveDir: NIOPath) = saveDir.resolve("practice")
  def officialRoomsDir(saveDir: NIOPath) = saveDir.resolve("official")

  /** Initialize the server state and make the HTTP server
    *
    * @param saveDir
    *   the directory to save the debate JSON files
    * @param blocker
    *   synchronous execution context for running server operations
    * @return
    *   the full HTTP app
    */
  def makeHttpApp(saveDir: NIOPath, blocker: Blocker) =
    for {
      qualityDataset <- QuALITYUtils.readQuALITY(qualityDataPath, blocker)
      trackedDebaters <- FileUtil
        .readJson[Set[String]](debatersSavePath(saveDir))
        .recoverWith { case e: Throwable =>
          IO {
            println("Error reading debaters JSON. Initializing to empty JSON.")
            println(s"--->\tError message: ${e.getMessage()}")
            Set.empty[String] // start with empty if none already exists
          }
        }
      trackedDebatersRef <- Ref[IO].of(trackedDebaters)
      pushUpdateRef      <- Ref[IO].of(IO.unit)
      officialDebates <- DebateStateManager
        .init(initializeDebate(qualityDataset), officialRoomsDir(saveDir), pushUpdateRef)
      practiceDebates <- DebateStateManager
        .init(initializeDebate(qualityDataset), practiceRoomsDir(saveDir), pushUpdateRef)
      officialRooms <- officialDebates.getRoomMetadata
      practiceRooms <- practiceDebates.getRoomMetadata
      // channel to update all clients on the lobby state
      leaderboard <- officialDebates.getLeaderboard
      allDebaters = officialRooms.unorderedFoldMap(_.roleAssignments.values.toSet)
      mainChannel <- Topic[IO, Lobby](
        Lobby(allDebaters, trackedDebaters, officialRooms, practiceRooms, leaderboard)
      )
      pushUpdate = {
        for {
          debaters      <- trackedDebatersRef.get
          officialRooms <- officialDebates.getRoomMetadata
          practiceRooms <- practiceDebates.getRoomMetadata
          leaderboard   <- officialDebates.getLeaderboard
          allDebaters = officialRooms.unorderedFoldMap(_.roleAssignments.values.toSet)
          _ <- mainChannel
            .publish1(Lobby(allDebaters, debaters, officialRooms, practiceRooms, leaderboard))
        } yield ()
      }
      _ <- pushUpdateRef.set(pushUpdate)
    } yield {
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

      // We need to configure CORS for the AJAX APIs if we're using a separate
      // endpoint for static file serving.
      // TODO: allow requests from our hostname instead of any
      // (but this requires us to know our hostname)
      // unless we set up Vite as a proxy
      val corsConfig = CORSConfig(
        anyOrigin = true,
        anyMethod = false,
        allowedMethods = Some(Set("GET", "POST")),
        allowCredentials = true,
        maxAge = 1.day.toSeconds
      )

      HttpsRedirect(
        Router(
          s"/$qualityServiceApiEndpoint" -> CORS(qualityService, corsConfig),
          "/" ->
            service(
              saveDir,
              mainChannel,
              trackedDebatersRef,
              officialDebates,
              practiceDebates,
              pushUpdate
            )
        )
      ).orNotFound
    }

  import org.http4s.dsl.io._

  object NameParam extends QueryParamDecoderMatcher[String]("name")

  /** Build the HTTP service (i.e., request -> IO[response]) running the
    * webapp's functionality.
    */
  def service(
    saveDir: NIOPath,              // where to save the debates as JSON
    mainChannel: Topic[IO, Lobby], // channel for updates to
    trackedDebaters: Ref[IO, Set[String]],
    officialDebates: DebateStateManager,
    practiceDebates: DebateStateManager,
    pushUpdate: IO[Unit]
  ) = {

    // Operations executed by the server

    def registerDebater(name: String) =
      for {
        _        <- trackedDebaters.update(_ + name)
        _        <- pushUpdate
        debaters <- trackedDebaters.get
        _        <- FileUtil.writeJson(debatersSavePath(saveDir))(debaters)
      } yield ()

    def removeDebater(name: String) =
      for {
        _        <- trackedDebaters.update(_ - name)
        _        <- pushUpdate
        debaters <- trackedDebaters.get
        _        <- FileUtil.writeJson(debatersSavePath(saveDir))(debaters)
      } yield ()

    val createLobbyWebsocket =
      for {
        debaters      <- trackedDebaters.get
        officialRooms <- officialDebates.getRoomMetadata
        practiceRooms <- practiceDebates.getRoomMetadata
        leaderboard   <- officialDebates.getLeaderboard
        allDebaters = officialRooms.unorderedFoldMap(_.roleAssignments.values.toSet)
        outStream = Stream
          .emit[IO, Option[Lobby]](
            Some(Lobby(allDebaters, debaters, officialRooms, practiceRooms, leaderboard))
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
                  case RegisterDebater(name) =>
                    registerDebater(name)
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
          onClose = IO.unit
        )
      } yield res

    // _root_ prefix because fs2 imports "io"
    // import _root_.io.circe.syntax._
    // import org.http4s.circe._ // for json encoder, per https://http4s.org/v0.19/json/

    HttpRoutes.of[IO] {
      // connect to the lobby to see open rooms / who's in them, etc.
      case GET -> Root / "main-ws" =>
        createLobbyWebsocket

      // Connect via websocket to the messaging channel for the given debate.
      // The participant is added to the debate state and then removed when the websocket closes.
      case GET -> Root / "official-ws" / roomName :? NameParam(participantName) =>
        officialDebates.createWebsocket(roomName, participantName)
      case GET -> Root / "practice-ws" / roomName :? NameParam(participantName) =>
        practiceDebates.createWebsocket(roomName, participantName)

    }
  }

  /** Get the info necessary to host using HTTPS. Looks for `keystore.jks` and
    * `password` files in the JVM resources. (Probably better to redirect from
    * an HTTPS reverse proxy instead)
    */
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
}
