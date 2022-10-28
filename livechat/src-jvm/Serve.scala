package livechat

import java.io.InputStream
import java.security.{ SecureRandom, KeyStore }
import javax.net.ssl.{ SSLContext, TrustManagerFactory, KeyManagerFactory }

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._

import scala.util.{ Try, Success, Failure }

import com.monovore.decline._
import com.monovore.decline.effect._
import _root_.org.http4s.server.middleware.HttpsRedirect

import io.circe.syntax._

import jjm.io.FileUtil
import java.nio.file.{Path => NIOPath}
import jjm.io.HttpUtil
import jjm.DotKleisli

object Serve extends CommandIOApp(
  name = "mill -i livechat.jvm.runMain livechat.jitsi.Serve",
  header = "Serve the live chat app.") {

  val portO = Opts.option[Int](
    "port", metavar = "<port number>", help = "Port where to host the server."
  )

  val saveO = Opts.option[NIOPath](
    "save", metavar = "<directory path>", help = "Directory in which to save the debates."
  )

  val sslO = Opts.flag(
    "ssl", help = "Whether to use SSL encryption/host over HTTPS."
  ).orFalse

  def main: Opts[IO[ExitCode]] = {
    (portO, saveO, sslO).mapN { (port, save, ssl) =>
      for {
        builder <- (
          if(!ssl) IO.pure(BlazeServerBuilder[IO]) else {
            getSslContext.attempt >>= {
              case Right(sslContext) => IO.pure(
                BlazeServerBuilder[IO]
                  .withSslContext(sslContext)
              )
              case Left(e) =>
                IO(System.err.println(s"HTTPS Configuration failed: ${e.getMessage}")).as(
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
              .serve.compile.drain
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

  @Lenses case class DebateRoom(
    debate: DebateState,
    channel: Topic[IO, DebateState])
  object DebateRoom {
    def create(debate: DebateState = DebateState.init) =
      Topic[IO, DebateState](debate).map(DebateRoom(debate, _))
  }

  def sortedRoomList(rooms: Map[String, DebateRoom]) = {
    rooms.toVector
      .sortBy(_._2.debate.debate.turns.view.flatMap(_.timestamp).lastOption.fold(1L)(-_))
      .map(_._1)
  }

  def makeHttpApp(saveDir: NIOPath, blocker: Blocker) = {
    val saveDirOs = os.Path(saveDir, os.pwd)
    for {
      _ <- IO(os.makeDir.all(saveDirOs))
      files <- IO(os.list(saveDirOs).map(_.toNIO).filter(_.toString.endsWith(".json")).toVector)
      // chatChannel <- Topic[IO, WebSocketFrame](Ping())
      rooms <- files.traverse(path =>
        FileUtil.readJson[DebateState](path)
          .flatMap(DebateRoom.create)
          .map { room =>
            val roomName = path.getFileName.toString.dropRight(".json".length)
            roomName -> room
          }
      ).map(_.toMap)
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
      case _ => true
    }

  // def sendWebSocketMessage[A: Pickler](message: A, queue: Queue[IO, WebSocketFrame]) = {
  //   queue.enqueue1(pickleToWSFrame(message))
  // }

  // val timeUpdateStream: Stream[IO, AdminMessage] = Stream
  //   .awakeEvery[IO](60.seconds)
  //   .map(d => GeneralAdminMessage(s"The time is now ${new java.util.Date()}"): AdminMessage)

  def roomMembersL(roomName: String) = Optics.at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)
    .composeLens(DebateState.participants)

  def roomStateL(roomName: String) = Optics.at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
      .composePrism(StdOptics.some[DebateRoom])
      .composeLens(DebateRoom.debate)

  import org.http4s.dsl.io._

  sealed trait DebateApiRequest { type Out }
  object DebateApiRequest {
    case object ListRooms extends DebateApiRequest {
      type Out = Vector[String]
    }
  }

  // def api(
  //   rooms: Ref[IO, Map[String, DebateRoom]]
  // ): DotKleisli[IO, DebateApiRequest] = new DotKleisli[IO, DebateApiRequest] {
  //   import DebateApiRequest._
  //   override def apply(req: DebateApiRequest): IO[req.Out] = req match {
  //     case ListRooms => rooms.get.map(
  //       _.toVector.sortBy(_._2.debate.debate.turns.view.flatMap(_.timestamp).lastOption.fold(1L)(-_)).map(_._1)
  //     ).asInstanceOf[IO[req.Out]]
  //   }
  // }

  object NameParam extends QueryParamDecoderMatcher[String]("name")
  object ParticipantIdParam extends QueryParamDecoderMatcher[String]("participantId")
  def service(
    saveDir: NIOPath,
    mainChannel: Topic[IO, Vector[String]],
    rooms: Ref[IO, Map[String, DebateRoom]],
    blocker: Blocker
  ) = {
    // TODO make this concurrency-safe
    def ensureChat(roomName: String) = for {
      roomOpt <- rooms.get.map(_.get(roomName))
      _ <- if(roomOpt.nonEmpty) IO.unit else DebateRoom.create().flatMap(room =>
        rooms.update(rooms =>
          if(rooms.contains(roomName)) rooms else rooms + (roomName -> room)
        ) >> getRoomList.flatMap(mainChannel.publish1)
      )
    } yield ()

    def addParticipant(roomName: String, participantId: String) = for {
      _ <- ensureChat(roomName)
      _ <- rooms.update(
        roomStateL(roomName).modify(_.addRole(ParticipantId(participantId, Observer)))
      )
      room <- rooms.get.map(_.apply(roomName))
      _ <- room.channel.publish1(room.debate)
    } yield ()

    def exitChat(roomName: String, participantId: String) = for {
      _ <- rooms.update(roomMembersL(roomName).modify(_.filter(_.name != participantId)))
      room <- rooms.get.map(_.apply(roomName))
      _ <- room.channel.publish1(room.debate)
    } yield ()

    def processUpdate(roomName: String, debateState: DebateState) = for {
      _ <- rooms.update(roomStateL(roomName).set(debateState))
      _ <- FileUtil.writeJson(saveDir.resolve(roomName + ".json"))(debateState)
      // room <- rooms.get.map(_.apply(roomName))
    } yield debateState

    def getRoomList = rooms.get.map(sortedRoomList)

    HttpRoutes.of[IO] {
      case req @ GET -> Root =>
        Ok(livechat.Page().render, Header("Content-Type", "text/html"))

      // case req @ GET -> Root / roomName =>
      //   Ok(livechat.Page(roomName).render, Header("Content-Type", "text/html"))

      case req @ GET -> Root / "download" / roomName =>
        rooms.get.map(_.get(roomName)).flatMap { 
          case None => NotFound()
          case Some(room) => Ok(
            room.debate.asJson.spaces2,
            Header("Content-Type", "application/json")
          )
        }
        
      case req @ GET -> Root / "main-ws" =>
        for {
          messageQueue <- Queue.bounded[IO, Unit](100)
          rooms <- getRoomList
          outStream = (
            Stream.emit[IO, Vector[String]](rooms)
              .merge(messageQueue.dequeue.evalMap(_ => getRoomList))
              .merge(mainChannel.subscribe(100))
              .map(pickleToWSFrame(_))
              .through(filterCloseFrames)
              // .merge(timeUpdateStream)
          )
          res <- WebSocketBuilder[IO].build(
            send = outStream,
            receive = x => {
              x.through(filterCloseFrames)
                .map(unpickleFromWSFrame[Unit])
                .through(messageQueue.enqueue)
            },
            onClose = IO.unit // exitChat(roomName, participantId)
          )
        } yield res

      // case req @ GET -> Root / "chat" :? NameParam(name) =>

      //   enterChat(name) >> WebSocketBuilder[IO].build(
      //     send = chatChannel.subscribe(100).through(filterCloseFrames),
      //     receive = chatChannel.publish,
      //     onClose = exitChat(name))

      case req @ GET -> Root / "ws" / roomName :? ParticipantIdParam(participantId) =>
        for {
          _ <- addParticipant(roomName, participantId)
          room <- rooms.get.map(_.apply(roomName))
          outStream = (
            Stream.emit[IO, DebateState](room.debate)
              .merge(room.channel.subscribe(100))
              .map(pickleToWSFrame(_))
              .through(filterCloseFrames)
              // .merge(timeUpdateStream)
          )
          res <- WebSocketBuilder[IO].build(
            send = outStream,
            receive = x => room.channel.publish(
              filterCloseFrames(x)
                .map(unpickleFromWSFrame[DebateState])
                .evalMap(processUpdate(roomName, _))
            ),
            onClose = exitChat(roomName, participantId)
          )
        } yield res

      // case req @ GET -> Root / "chat" :? NameParam(name) =>

      //   enterChat(name) >> WebSocketBuilder[IO].build(
      //     send = chatChannel.subscribe(100).through(filterCloseFrames),
      //     receive = chatChannel.publish,
      //     onClose = exitChat(name))

      case req @ GET -> Root / "file" / path =>
        StaticFile.fromResource("/" + path, blocker, Some(req)).getOrElseF(NotFound())
    }
  }

  val getSslContext = for {
    password <- IO(
      new java.util.Scanner(
        getClass.getClassLoader.getResourceAsStream("password")
      ).next.toCharArray
    )
    keystore <- IO {
      val keystoreInputStream: InputStream = getClass.getClassLoader.getResourceAsStream("keystore.jks")
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
