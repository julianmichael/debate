package debate

import java.nio.file.{Path => NIOPath}

import cats.kernel.Order

import cats.implicits._

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.effect.Concurrent

import monocle.function.{all => Optics}
import monocle.std.{all => StdOptics}
import monocle.macros.Lenses

import fs2.concurrent.Topic
import fs2.Stream

import org.http4s.server.websocket._

import jjm.io.FileUtil
import java.nio.file.Files

import scala.concurrent.duration._
import cats.effect.Timer

/** The server state for a debate room.
  *
  * @param debate
  *   the full contents of a debate and current set of participants on the page.
  * @param channel
  *   the channel on which to send debate state updates to all participants
  */
@Lenses case class DebateRoom(
    debate: DebateState,
    channel: Topic[IO, DebateState]
)
object DebateRoom {
  def create(debate: DebateState = DebateState.init)(implicit
      c: Concurrent[IO]
  ) =
    Topic[IO, DebateState](debate).map(DebateRoom(debate, _))

  /** Order: 1) new uninitialized rooms, 2) latest turns taken
    */
  implicit val debateRoomOrder: Order[DebateRoom] = Order.by(
    _.debate.debate match {
      case None => 0L
      case Some(debate) =>
        debate.rounds.view
          .flatMap(_.timestamp(debate.setup.numDebaters))
          .lastOption
          .fold(1L)(-_)
    }
  )
}

case class DebateStateManager(
    initializeDebate: DebateSetupSpec => IO[DebateSetup],
    rooms: Ref[IO, Map[String, DebateRoom]],
    saveDir: NIOPath,
    pushUpdateRef: Ref[IO, IO[Unit]]
)(implicit
    c: Concurrent[IO]
) {

  private def roomMembersL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)
    .composeLens(DebateState.participants)

  private def roomStateL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)

  def getRoomList = rooms.get.map {
    _.toVector
      .sortBy(_._2)
      .map { case (roomName, room) =>
        RoomMetadata(
          roomName,
          room.debate.debate.unorderedFoldMap(_.setup.roles.values.toSet),
          room.debate.participants.map(_.name),
          room.debate.status
        )
      }
  }

  def pushUpdate = pushUpdateRef.get.flatten

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
          ) >> pushUpdate
        )
    )
  } yield ()

  def deleteDebate(roomName: String) = for {
    _ <- rooms.update(_ - roomName)
    _ <- IO(Files.delete(saveDir.resolve(roomName + ".json")))
    _ <- pushUpdate
  } yield ()

  def addParticipant(roomName: String, participantId: String) = for {
    _ <- ensureDebate(roomName)
    _ <- rooms.update(
      roomStateL(roomName).modify { debateState =>
        val role = debateState.debate
          .flatMap(debate =>
            debate.setup.roles
              .find(_._2 == participantId)
              .map(_._1)
          )
          .getOrElse(Observer)
        debateState.addParticipant(ParticipantId(participantId, role))
      }
    )
    room <- rooms.get.map(_.apply(roomName))
    _ <- room.channel.publish1(room.debate)
    _ <- pushUpdate
  } yield ()

  def removeParticipant(roomName: String, participantId: String) = for {
    _ <- rooms.update(
      roomMembersL(roomName).modify(_.filter(_.name != participantId))
    )
    room <- rooms.get.map(_.apply(roomName))
    _ <- room.channel.publish1(room.debate)
    _ <- IO(room.debate.participants.isEmpty && room.debate.debate.isEmpty).ifM(
      rooms.update(_ - roomName),
      IO.unit
    )
    _ <- pushUpdate
  } yield ()

  def processUpdate(roomName: String, request: DebateStateUpdateRequest) = {
    val updateState = request match {
      case DebateStateUpdateRequest.State(debateState) =>
        rooms.update(roomStateL(roomName).set(debateState)) // update state
      case DebateStateUpdateRequest.SetupSpec(setupSpec) => {
        initializeDebate(setupSpec).flatMap { setup =>
          rooms.update(
            roomStateL(roomName)
              .composeLens(DebateState.debate)
              .set(Some(Debate(setup, Vector())))
          )
        }
      }
    }

    for {
      _ <- updateState
      debateState <- rooms.get.map(_.apply(roomName).debate)
      _ <- debateState.debate.traverse(
        FileUtil.writeJson(saveDir.resolve(roomName + ".json"))
      ) // save after every change
      // TODO: maybe update clients on the new room list since room order has changed? Or unnecessary computation?
      // _ <- getRoomList.flatMap(mainChannel.publish1) // update all clients on the new room list
    } yield debateState

  }

  def createWebsocket(roomName: String, participantName: String)(implicit
      timer: Timer[IO]
  ) = for {
    _ <- addParticipant(roomName, participantName)
    room <- rooms.get.map(_.apply(roomName))
    outStream = (
      Stream
        .emit[IO, Option[DebateState]](Some(room.debate))
        .merge(room.channel.subscribe(100).map(Some(_)))
        .merge(Stream.awakeEvery[IO](30.seconds).map(_ => None))
        .map(pickleToWSFrame(_))
        .through(filterCloseFrames)
    )
    res <- WebSocketBuilder[IO].build(
      send = outStream,
      receive = x =>
        room.channel.publish(
          filterCloseFrames(x)
            .map(unpickleFromWSFrame[DebateStateUpdateRequest])
            .evalMap(processUpdate(roomName, _))
        ),
      onClose = removeParticipant(roomName, participantName)
    )
  } yield res

  def toLeaderboard = {
    rooms.get.map({ roomMap =>
      Leaderboard.fromDebates(
        roomMap.values.toList
          .flatMap(_.debate.debate.toList)
      )
    })
  }

}
object DebateStateManager {
  def init(
      initializeDebate: DebateSetupSpec => IO[DebateSetup],
      saveDir: NIOPath,
      pushUpdateRef: Ref[IO, IO[Unit]]
  )(implicit c: Concurrent[IO]) = {
    val saveDirOs = os.Path(saveDir, os.pwd)
    for {
      _ <- IO(os.makeDir.all(saveDirOs))
      files <- IO(
        os.list(saveDirOs)
          .map(_.toNIO)
          .filter(_.toString.endsWith(".json"))
          .toVector
      )
      rooms <- files
        .traverse(path =>
          FileUtil
            .readJson[Debate](path)
            .flatMap(debate =>
              DebateRoom.create(DebateState(Some(debate), Set()))
            )
            .map { room =>
              val roomName = path.getFileName.toString.dropRight(".json".length)
              roomName -> room
            }
        )
        .map(_.toMap)
      roomsRef <- Ref[IO].of(rooms)
    } yield DebateStateManager(
      initializeDebate,
      roomsRef,
      saveDir,
      pushUpdateRef
    )
  }
}
