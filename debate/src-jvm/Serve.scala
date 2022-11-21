package debate

import java.io.InputStream
import java.security.{SecureRandom, KeyStore}
import javax.net.ssl.{SSLContext, TrustManagerFactory, KeyManagerFactory}

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import _root_.org.http4s.server.middleware.HttpsRedirect

import io.circe.syntax._

import jjm.io.FileUtil
import java.nio.file.{Path => NIOPath}
import debate.RoomMetadata
import debate.RegisterDebater

// NOTE: for json encode/decode if we end up wanting it
// import _root_.io.circe.parser.{decode => circeDecode}
// import _root_.io.circe.Encoder
// import _root_.io.circe.Decoder

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object Serve
    extends CommandIOApp(
      name = "mill -i debate.jvm.run",
      header = "Serve the live chat app."
    ) {

  val jsDepsPathO = Opts.option[NIOPath](
    "jsDeps", metavar = "path", help = "Where to get the JS deps file."
  )

  val jsPathO = Opts.option[NIOPath](
    "js", metavar = "path", help = "Where to get the JS main file."
  )

  val portO = Opts.option[Int](
    "port",
    metavar = "<port number>",
    help = "Port where to host the server."
  )

  val saveO = Opts.option[NIOPath](
    "save",
    metavar = "<directory path>",
    help = "Directory in which to save the debates."
  )

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
    (jsPathO, jsDepsPathO, portO, saveO, sslO).mapN { (jsPath, jsDepsPath, port, save, ssl) =>
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

  import fs2._
  import fs2.concurrent.Topic

  import org.http4s._
  // import org.http4s.circe._
  import org.http4s.server.websocket._
  import org.http4s.websocket.WebSocketFrame
  import org.http4s.websocket.WebSocketFrame._

  import monocle.function.{all => Optics}
  import monocle.std.{all => StdOptics}
  import monocle.macros._

  /** The server state for a debate room.
    *
    * @param debate
    *   the full contents of a debate and current set of participants on the
    *   page.
    * @param channel
    *   the channel on which to send debate state updates to all participants
    */
  @Lenses case class DebateRoom(
      debate: DebateState,
      channel: Topic[IO, DebateState]
  )
  object DebateRoom {
    def create(debate: DebateState = DebateState.init) =
      Topic[IO, DebateState](debate).map(DebateRoom(debate, _))
  }

  /** Order:
    * 1) new uninitialized rooms,
    * 2) latest turns taken
    */
  def getDebateRoomSortKey(room: DebateRoom): Long = {
    room.debate.debate match {
      case None => 0L
      case Some(debate) =>
        debate.turns.view
          .flatMap(_.timestamp)
          .lastOption
          .fold(1L)(-_)
    }
  }

  def sortedRoomList(rooms: Map[String, DebateRoom]) = {
    rooms.toVector.sortBy(t => getDebateRoomSortKey(t._2))
      .map { case (roomName, room) =>
        RoomMetadata(
          roomName,
          room.debate.participants.map(_.name),
          room.debate.status)
      }
  }

  def loadRooms(saveDir: NIOPath) = {
    val saveDirOs = os.Path(saveDir, os.pwd)
    for {
      _ <- IO(os.makeDir.all(saveDirOs))
      files <- IO(
        os.list(saveDirOs)
          .map(_.toNIO)
          .filter(_.toString.endsWith(".json"))
          .toVector
      )
      loaded <- files
        .traverse(path =>
          FileUtil
            .readJson[Debate](path)
            .flatMap(debate => DebateRoom.create(DebateState(Some(debate), Set())))
            .map { room =>
              val roomName = path.getFileName.toString.dropRight(".json".length)
              roomName -> room
            }
        )
        .map(_.toMap)
    } yield loaded
  }


  def debatersSavePath(saveDir: NIOPath) = saveDir.resolve("debaters.json")

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
      trackedDebaters <- FileUtil.readJson[Set[String]](debatersSavePath(saveDir)).recoverWith {
        case e: Throwable => IO {
          println("Error reading debaters JSON. Initializing to empty JSON.")
          e.printStackTrace()
          Set.empty[String] // start with empty if none already exists
        }
      }
      rooms <- loadRooms(saveDir.resolve("open"))
      _ <- IO(println(s"load: $rooms"))
      mainChannel <- Topic[IO, Lobby](Lobby(trackedDebaters, sortedRoomList(rooms)))
      // _ <- mainChannel.publish1(RoomsUpdate(sortedRoomList(rooms)))
      trackedDebatersRef <- Ref[IO].of(trackedDebaters)
      roomsRef <- Ref[IO].of(rooms)
    } yield {
      HttpsRedirect(
        Router(
          "/" -> service(jsPath, jsDepsPath, saveDir, mainChannel, trackedDebatersRef, roomsRef, blocker)
        )
      ).orNotFound
    }
  }

  // NOTE: maybe change to json pickling later if reading network message is a pain.
  // haven't bothered with this yet because not sure if it will break something.

  // def jsonPickleToWSFrame[A: Encoder](message: A): WebSocketFrame = {
  //     Text(message.asJson.noSpaces)
  //   }
  // def jsonUnpickleFromWSFrame[A: Decoder](frame: WebSocketFrame): IO[A] = {
  //   for {
  //     str <- IO.fromEither(frame.data.decodeUtf8)
  //     res <- IO.fromEither(circeDecode[A](str))
  //   } yield res
  // }

  import boopickle.Default._

  def pickleToWSFrame[A: Pickler](message: A): WebSocketFrame = {
    Binary(scodec.bits.ByteVector.view(Pickle.intoBytes(message)))
  }
  def unpickleFromWSFrame[A: Pickler](frame: WebSocketFrame): A = {
    Unpickle[A].fromBytes(frame.data.toByteBuffer)
  }

  val filterCloseFrames: Pipe[IO, WebSocketFrame, WebSocketFrame] =
    _.filter {
      case Close(_) => false
      case _        => true
    }

  // Optics for debate state fields

  def roomMembersL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)
    .composeLens(DebateState.participants)

  def roomStateL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)

  import org.http4s.dsl.io._

  object NameParam extends QueryParamDecoderMatcher[String]("name")
  object ParticipantIdParam
      extends QueryParamDecoderMatcher[String]("participantId")

  /** Build the HTTP service (i.e., request -> IO[response]) running the
    * webapp's functionality.
    */
  def service(
      jsPath: NIOPath,
      jsDepsPath: NIOPath,
      saveDir: NIOPath, // where to save the debates as JSON
      mainChannel: Topic[IO, Lobby], // channel for updates to
      trackedDebaters: Ref[IO, Set[String]],
      rooms: Ref[IO, Map[String, DebateRoom]],
      blocker: Blocker
  ) = {

    // Operations executed by the server

    def getRoomList = rooms.get.map(sortedRoomList)

    // update all clients on the lobby state
    def publishLobbyState = for {
      debaters <- trackedDebaters.get
      roomList <- getRoomList
      _ <- mainChannel.publish1(Lobby(debaters, roomList))
    } yield ()

    def registerDebater(name: String) = for {
      _ <- trackedDebaters.update(_ + name)
      _ <- publishLobbyState
      debaters <- trackedDebaters.get
      _ <- FileUtil.writeJson(debatersSavePath(saveDir))(debaters)
    } yield ()

    // Ensure a debate room is present (initialize if necessary)
    def ensureDebate(roomName: String) = for {
      roomOpt <- rooms.get.map(_.get(roomName))
      _ <- IO(roomOpt.nonEmpty).ifM(
        ifTrue = IO.unit,
        ifFalse = DebateRoom
          .create()
          .flatMap(room =>
            rooms.update(rooms =>
              if (rooms.contains(roomName)) rooms
              else rooms + (roomName -> room)
            ) >> publishLobbyState
          )
      )
    } yield ()

    def addParticipant(roomName: String, participantId: String) = for {
      _ <- ensureDebate(roomName)
      _ <- rooms.update(
        roomStateL(roomName).modify(
          _.addParticipant(ParticipantId(participantId, Observer))
        )
      )
      room <- rooms.get.map(_.apply(roomName))
      _ <- room.channel.publish1(room.debate)
      _ <- publishLobbyState
    } yield ()

    def removeParticipant(roomName: String, participantId: String) = for {
      _ <- rooms.update(
        roomMembersL(roomName).modify(_.filter(_.name != participantId))
      )
      room <- rooms.get.map(_.apply(roomName))
      _ <- room.channel.publish1(room.debate)
      _ <- IO(room.debate.participants.isEmpty && room.debate.debate.isEmpty).ifM(
        rooms.update(_ - roomName), IO.unit
      )
      _ <- publishLobbyState
    } yield ()

    def processUpdate(roomName: String, debateState: DebateState) = for {
      _ <- rooms.update(roomStateL(roomName).set(debateState)) // update state
      _ <- debateState.debate.traverse(
        FileUtil.writeJson(saveDir.resolve("open").resolve(roomName + ".json"))
      ) // save after every change
        // TODO: maybe update clients on the new room list since room order has changed? Or unnecessary computation?
      // _ <- getRoomList.flatMap(mainChannel.publish1) // update all clients on the new room list
    } yield debateState

    val jsDepsLocation = "deps.js"
    val jsLocation = "out.js"
    val jsMapLocation = jsLocation + ".map"

    HttpRoutes.of[IO] {
      // Land on the actual webapp.
      case GET -> Root =>
        Ok(debate.Page(jsDepsLocation = jsDepsLocation, jsLocation = jsLocation).render, Header("Content-Type", "text/html"))

      // Download the JSON for a debate.
      case GET -> Root / "download" / roomName =>
        rooms.get.map(_.get(roomName)).flatMap {
          case None => NotFound()
          case Some(room) =>
            Ok(
              room.debate.asJson.spaces2,
              Header("Content-Type", "application/json")
            )
        }

      // Connect via websocket to the 'main' channel which sends updates to everyone.
      // This is still WIP.
      case GET -> Root / "main-ws" =>
        for {
          debaters <- trackedDebaters.get
          rooms <- getRoomList
          outStream = (
            Stream
              .emit[IO, Lobby](
                Lobby(debaters, rooms)
              ) // send the current set of debaters and rooms on connect
              .merge(
                mainChannel.subscribe(100)
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
                }
            },
            onClose = IO.unit
          )
        } yield res

      // Connect via websocket to the messaging channel for the given debate.
      // The participant is added to the debate state and then removed when the websocket closes.
      case GET -> Root / "ws" / roomName :? ParticipantIdParam(
            participantId
          ) =>
        for {
          _ <- addParticipant(roomName, participantId)
          room <- rooms.get.map(_.apply(roomName))
          outStream = (
            Stream
              .emit[IO, DebateState](room.debate)
              .merge(room.channel.subscribe(100))
              .map(pickleToWSFrame(_))
              .through(filterCloseFrames)
            // .merge(timeUpdateStream)
          )
          res <- WebSocketBuilder[IO].build(
            send = outStream,
            receive = x =>
              room.channel.publish(
                filterCloseFrames(x)
                  .map(unpickleFromWSFrame[DebateState])
                  .evalMap(processUpdate(roomName, _))
              ),
            onClose = removeParticipant(roomName, participantId)
          )
        } yield res

      case req @ GET -> Root / `staticFilePrefix` / `jsDepsLocation` =>
        StaticFile.fromString(jsDepsPath.toString, blocker, Some(req))
          .getOrElseF(NotFound())
      case req @ GET -> Root / `staticFilePrefix` / `jsLocation` =>
        StaticFile.fromString(jsPath.toString, blocker, Some(req))
          .getOrElseF(NotFound())
      case req @ GET -> Root / `staticFilePrefix` / `jsMapLocation` =>
        StaticFile.fromString(jsPath.toString + ".map", blocker, Some(req))
          .getOrElseF(NotFound())
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
