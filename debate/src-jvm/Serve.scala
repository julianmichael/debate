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

import scala.util.{Try, Success, Failure}

import com.monovore.decline._
import com.monovore.decline.effect._
import _root_.org.http4s.server.middleware.HttpsRedirect

import io.circe.syntax._

import jjm.io.FileUtil
import java.nio.file.{Path => NIOPath}
import jjm.io.HttpUtil
import jjm.DotKleisli

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object Serve
    extends CommandIOApp(
      name = "mill -i debate.jvm.runMain debate.Serve",
      header = "Serve the live chat app."
    ) {

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

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = {
    (portO, saveO, sslO).mapN { (port, save, ssl) =>
      for {
        builder <-
          (
            if (!ssl) IO.pure(BlazeServerBuilder[IO])
            else {
              getSslContext.attempt >>= {
                case Right(sslContext) =>
                  IO.pure(
                    BlazeServerBuilder[IO]
                      .withSslContext(sslContext)
                  )
                case Left(e) =>
                  IO(
                    System.err.println(
                      s"HTTPS Configuration failed: ${e.getMessage}"
                    )
                  ).as(
                    BlazeServerBuilder[IO]
                  )
              }
            }
          )
        _ <- Blocker[IO].use { blocker =>
          makeHttpApp(save, blocker).flatMap(app =>
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

  import scala.concurrent.duration._
  import scala.language.postfixOps

  import fs2._
  import fs2.concurrent.Queue
  import fs2.concurrent.Topic

  import org.http4s._
  import org.http4s.headers._
  import org.http4s.server.staticcontent._
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

  def sortedRoomList(rooms: Map[String, DebateRoom]) = {
    rooms.toVector
      .sortBy(
        _._2.debate.debate.turns.view
          .flatMap(_.timestamp)
          .lastOption
          .fold(1L)(-_)
      )
      .map(_._1)
  }

  /** Initialize the server state and make the HTTP server
    *
    * @param saveDir
    *   the directory to save the debate JSON files
    * @param blocker
    *   synchronous execution context for running server operations
    * @return
    *   the full HTTP app
    */
  def makeHttpApp(saveDir: NIOPath, blocker: Blocker) = {
    val saveDirOs = os.Path(saveDir, os.pwd)
    for {
      _ <- IO(os.makeDir.all(saveDirOs))
      files <- IO(
        os.list(saveDirOs)
          .map(_.toNIO)
          .filter(_.toString.endsWith(".json"))
          .toVector
      )
      // chatChannel <- Topic[IO, WebSocketFrame](Ping())
      rooms <- files
        .traverse(path =>
          FileUtil
            .readJson[DebateState](path)
            .flatMap(DebateRoom.create)
            .map { room =>
              val roomName = path.getFileName.toString.dropRight(".json".length)
              roomName -> room
            }
        )
        .map(_.toMap)
      _ <- IO(println(s"load: $rooms"))
      mainChannel <- Topic[IO, Vector[String]](sortedRoomList(rooms))
      roomsRef <- Ref[IO].of(rooms)
    } yield {
      HttpsRedirect(
        Router(
          // "/api" -> HttpUtil.makeHttpPostServer(api(roomsRef)),
          "/" -> service(saveDir, mainChannel, roomsRef, blocker)
        )
      ).orNotFound
    }
  }

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

  // val timeUpdateStream: Stream[IO, AdminMessage] = Stream
  //   .awakeEvery[IO](60.seconds)
  //   .map(d => GeneralAdminMessage(s"The time is now ${new java.util.Date()}"): AdminMessage)

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
      saveDir: NIOPath, // where to save the debates as JSON
      mainChannel: Topic[IO, Vector[String]], // channel for updates to
      rooms: Ref[IO, Map[String, DebateRoom]],
      blocker: Blocker
  ) = {

    // Operations executed by the server

    // Ensure a debate room is present (initialize if necessary)
    def ensureDebate(roomName: String) = for {
      roomOpt <- rooms.get.map(_.get(roomName))
      _ <-
        if (roomOpt.nonEmpty) IO.unit
        else
          DebateRoom
            .create()
            .flatMap(room =>
              rooms.update(rooms =>
                if (rooms.contains(roomName)) rooms
                else rooms + (roomName -> room)
              ) >> getRoomList.flatMap(
                mainChannel.publish1
              ) // update all clients on the list of rooms
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
    } yield ()

    def removeParticipant(roomName: String, participantId: String) = for {
      _ <- rooms.update(
        roomMembersL(roomName).modify(_.filter(_.name != participantId))
      )
      room <- rooms.get.map(_.apply(roomName))
      _ <- room.channel.publish1(room.debate)
    } yield ()

    def processUpdate(roomName: String, debateState: DebateState) = for {
      _ <- rooms.update(roomStateL(roomName).set(debateState)) // update state
      _ <- FileUtil.writeJson(saveDir.resolve(roomName + ".json"))(
        debateState
      ) // save after every change
      // TODO: maybe update clients on the new room list since room order has changed? Or unnecessary computation?
      // _ <- getRoomList.flatMap(mainChannel.publish1) // update all clients on the new room list
    } yield debateState

    def getRoomList = rooms.get.map(sortedRoomList)

    HttpRoutes.of[IO] {
      // Land on the actual webapp.
      case req @ GET -> Root =>
        Ok(debate.Page().render, Header("Content-Type", "text/html"))

      // Download the JSON for a debate.
      case req @ GET -> Root / "download" / roomName =>
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
      case req @ GET -> Root / "main-ws" =>
        for {
          // messageQueue <- Queue.bounded[IO, Unit](100)
          rooms <- getRoomList
          outStream = (
            Stream
              .emit[IO, Vector[String]](
                rooms
              ) // send the current set of rooms on connect
              // .merge(messageQueue.dequeue.evalMap(_ => getRoomList)) // send any further updates
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
                .map(
                  unpickleFromWSFrame[Unit]
                ) // clients can only 'ping' this websocket. does nothing.
              // .through(messageQueue.enqueue) // -> triggers everyone to refresh the rooms (currently unused).
            },
            onClose = IO.unit
          )
        } yield res

      // Connect via websocket to the messaging channel for the given debate.
      // The participant is added to the debate state and then removed when the websocket closes.
      case req @ GET -> Root / "ws" / roomName :? ParticipantIdParam(
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

      // serve static files. Used for serving the JS to the client.
      case req @ GET -> Root / staticFilePrefix / path =>
        StaticFile
          .fromResource("/" + path, blocker, Some(req))
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
