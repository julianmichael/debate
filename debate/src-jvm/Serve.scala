package debate

// fs2 imports "io" below
import io.circe.syntax._

import debate.quality._

import java.io.InputStream
import java.security.{SecureRandom, KeyStore}
import javax.net.ssl.{SSLContext, TrustManagerFactory, KeyManagerFactory}

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import org.http4s._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import _root_.org.http4s.server.middleware.HttpsRedirect

import jjm.implicits._
import jjm.io.FileUtil
import java.nio.file.{Path => NIOPath}

import fs2._
import fs2.concurrent.Topic
import java.nio.file.Paths
import jjm.io.HttpUtil
import jjm.DotKleisli

import scala.concurrent.duration._

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object Serve
    extends CommandIOApp(
      name = "mill -i debate.jvm.run",
      header = "Serve the live chat app."
    ) {

  val jsDepsPathO = Opts.option[NIOPath](
    "jsDeps",
    metavar = "path",
    help = "Where to get the JS deps file."
  )

  val jsPathO = Opts.option[NIOPath](
    "js",
    metavar = "path",
    help = "Where to get the JS main file."
  )

  val portO = Opts
    .option[Int](
      "port",
      metavar = "<port number>",
      help = "Port where to host the server."
    )
    .withDefault(8080)

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "<directory path>",
      help = "Directory in which to save the debates."
    )
    .withDefault(Paths.get("save"))

  val sslO = Opts
    .flag(
      "ssl",
      help = "Whether to use SSL encryption/host over HTTPS."
    )
    .orFalse

  def getBuilder(ssl: Boolean) = {
    if (!ssl) IO.pure(BlazeServerBuilder[IO](executionContext))
    else {
      getSslContext.attempt >>= {
        case Right(sslContext) =>
          IO.pure(
            BlazeServerBuilder[IO](executionContext)
              .withSslContext(sslContext)
          )
        case Left(e) =>
          IO(
            System.err.println(
              s"HTTPS Configuration failed: ${e.getMessage}"
            )
          ).as(
            BlazeServerBuilder[IO](executionContext)
          )
      }
    }
  }

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = {
    (jsPathO, jsDepsPathO, portO, saveO, sslO).mapN {
      (jsPath, jsDepsPath, port, save, ssl) =>
        for {
          builder <- getBuilder(ssl)
          _ <- Blocker[IO].use { blocker =>
            makeHttpApp(jsPath, jsDepsPath, save, blocker).flatMap(app =>
              builder
                .bindHttp(port, "0.0.0.0")
                .withHttpApp(app)
                .serve
                .compile
                .drain
            )
          }
        } yield ExitCode.Success
    }
  }

  def initializeDebate(
      qualityDataset: Map[String, QuALITYStory]
  )(setupSpec: DebateSetupSpec): IO[DebateSetup] = {
    val tokenize = simpleTokenize(_)
    val sourceMaterialIO = setupSpec.sourceMaterial match {
      case CustomSourceMaterialSpec(title, contents) =>
        IO.pure(CustomSourceMaterial(title, tokenize(contents)))
      case QuALITYSourceMaterialSpec(articleId) =>
        IO(qualityDataset(articleId)).map { qualityStory =>
          QuALITYSourceMaterial(
            articleId = articleId,
            title = qualityStory.title,
            contents = tokenize(qualityStory.article)
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

  val qualityDataPath = Paths.get("data")
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
  def makeHttpApp(
      jsPath: NIOPath,
      jsDepsPath: NIOPath,
      saveDir: NIOPath,
      blocker: Blocker
  ) = {
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
      pushUpdateRef <- Ref[IO].of(IO.unit)
      officialDebates <- DebateStateManager.init(
        initializeDebate(qualityDataset),
        officialRoomsDir(saveDir),
        pushUpdateRef
      )
      practiceDebates <- DebateStateManager.init(
        initializeDebate(qualityDataset),
        practiceRoomsDir(saveDir),
        pushUpdateRef
      )
      officialRooms <- officialDebates.getRoomList
      practiceRooms <- practiceDebates.getRoomList
      // channel to update all clients on the lobby state
      mainChannel <- Topic[IO, Lobby](
        Lobby(trackedDebaters, officialRooms, practiceRooms)
      )
      pushUpdate = {
        for {
          debaters <- trackedDebatersRef.get
          officialRoomList <- officialDebates.getRoomList
          practiceRoomList <- practiceDebates.getRoomList
          _ <- mainChannel.publish1(
            Lobby(debaters, officialRoomList, practiceRoomList)
          )
        } yield ()
      }
      _ <- pushUpdateRef.set(pushUpdate)
    } yield {
      val qualityService = HttpUtil.makeHttpPostServer(
        new DotKleisli[IO, QuALITYService.Request] {
          import QuALITYService.Request
          def apply(req: Request): IO[req.Out] = {
            val res = req match {
              case Request.GetIndex => IO(qualityDataset.mapVals(_.title))
              case Request.GetStory(articleId) => IO(qualityDataset(articleId))
            }
            // not sure why it isn't inferring the type...
            res.asInstanceOf[IO[req.Out]]
          }
        }
      )
      HttpsRedirect(
        Router(
          s"/$qualityServiceApiEndpoint" -> qualityService,
          "/" -> service(
            jsPath,
            jsDepsPath,
            saveDir,
            mainChannel,
            trackedDebatersRef,
            officialDebates,
            practiceDebates,
            pushUpdate,
            blocker
          )
        )
      ).orNotFound
    }
  }

  import org.http4s.dsl.io._

  object NameParam extends QueryParamDecoderMatcher[String]("name")

  /** Build the HTTP service (i.e., request -> IO[response]) running the
    * webapp's functionality.
    */
  def service(
      jsPath: NIOPath,
      jsDepsPath: NIOPath,
      saveDir: NIOPath, // where to save the debates as JSON
      mainChannel: Topic[IO, Lobby], // channel for updates to
      trackedDebaters: Ref[IO, Set[String]],
      officialDebates: DebateStateManager,
      practiceDebates: DebateStateManager,
      pushUpdate: IO[Unit],
      blocker: Blocker
  ) = {

    // Operations executed by the server

    def registerDebater(name: String) = for {
      _ <- trackedDebaters.update(_ + name)
      _ <- pushUpdate
      debaters <- trackedDebaters.get
      _ <- FileUtil.writeJson(debatersSavePath(saveDir))(debaters)
    } yield ()

    def removeDebater(name: String) = for {
      _ <- trackedDebaters.update(_ - name)
      _ <- pushUpdate
      debaters <- trackedDebaters.get
      _ <- FileUtil.writeJson(debatersSavePath(saveDir))(debaters)
    } yield ()

    val jsDepsLocation = "deps.js"
    val jsLocation = "out.js"
    val jsMapLocation = jsLocation + ".map"

    val createLobbyWebsocket = for {
      debaters <- trackedDebaters.get
      officialRooms <- officialDebates.getRoomList
      practiceRooms <- practiceDebates.getRoomList
      outStream = (
        Stream
          .emit[IO, Option[Lobby]](
            Some(Lobby(debaters, officialRooms, practiceRooms))
          ) // send the current set of debaters and rooms on connect
          .merge(
            Stream.awakeEvery[IO](30.seconds).map(_ => None)
          ) // send a heartbeat every 30s
          .merge(
            mainChannel.subscribe(100).map(Some(_))
          ) // and subscribe to the main channel
          .map(pickleToWSFrame(_))
          .through(
            filterCloseFrames
          ) // I'm not entirely sure why I remove the close frames.
      )
      res <- WebSocketBuilder[IO].build(
        send = outStream,
        receive = x => {
          x.through(filterCloseFrames)
            .map(unpickleFromWSFrame[MainChannelRequest])
            .evalMap {
              case RegisterDebater(name) => registerDebater(name)
              case RemoveDebater(name)   => removeDebater(name)
              case DeleteRoom(isOfficial, roomName) =>
                if (isOfficial) officialDebates.deleteDebate(roomName)
                else practiceDebates.deleteDebate(roomName)
            }
        },
        onClose = IO.unit
      )
    } yield res

    HttpRoutes.of[IO] {
      // Land on the actual webapp.
      case GET -> Root =>
        Ok(
          debate
            .Page(jsDepsLocation = jsDepsLocation, jsLocation = jsLocation)
            .render,
          Header("Content-Type", "text/html")
        )

      // connect to the lobby to see open rooms / who's in them, etc.
      case GET -> Root / "main-ws" => createLobbyWebsocket

      // Connect via websocket to the messaging channel for the given debate.
      // The participant is added to the debate state and then removed when the websocket closes.
      case GET -> Root / "official-ws" / roomName :? NameParam(
            participantName
          ) =>
        officialDebates.createWebsocket(roomName, participantName)
      case GET -> Root / "practice-ws" / roomName :? NameParam(
            participantName
          ) =>
        practiceDebates.createWebsocket(roomName, participantName)

      case req @ GET -> Root / `staticFilePrefix` / `jsDepsLocation` =>
        StaticFile
          .fromString(jsDepsPath.toString, blocker, Some(req))
          .getOrElseF(NotFound())
      case req @ GET -> Root / `staticFilePrefix` / `jsLocation` =>
        StaticFile
          .fromString(jsPath.toString, blocker, Some(req))
          .getOrElseF(NotFound())
      case req @ GET -> Root / `staticFilePrefix` / `jsMapLocation` =>
        StaticFile
          .fromString(jsPath.toString + ".map", blocker, Some(req))
          .getOrElseF(NotFound())

      case GET -> Root / "leaderboard" =>
        import org.http4s.circe._ // for json encoder, per https://http4s.org/v0.19/json/
        import Leaderboard._
        Ok(officialDebates.toLeaderboard.map(_.asJson))
    }
  }

  /** Get the info necessary to host using HTTPS. Looks for `keystore.jks` and
    * `password` files in the JVM resources. (Probably better to redirect from
    * an HTTPS reverse proxy instead)
    */
  val getSslContext = for {
    password <- IO(
      new java.util.Scanner(
        getClass.getClassLoader.getResourceAsStream("password")
      ).next.toCharArray
    )
    keystore <- IO {
      val keystoreInputStream: InputStream =
        getClass.getClassLoader.getResourceAsStream("keystore.jks")
      require(keystoreInputStream != null, "Keystore required!")
      val keystore: KeyStore = KeyStore.getInstance("jks")
      keystore.load(keystoreInputStream, password)
      keystore
    }
    sslContext <- IO {
      val keyManagerFactory: KeyManagerFactory =
        KeyManagerFactory.getInstance("SunX509")
      keyManagerFactory.init(keystore, password)

      val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
      tmf.init(keystore)

      val context = SSLContext.getInstance("TLS")
      context.init(
        keyManagerFactory.getKeyManagers,
        tmf.getTrustManagers,
        new SecureRandom
      )
      context
    }
  } yield sslContext
}
