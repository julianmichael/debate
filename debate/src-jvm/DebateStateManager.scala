package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import scala.concurrent.duration._

import cats.effect.Concurrent
import cats.effect.IO
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.implicits._
import cats.kernel.Order

import fs2.Stream
import fs2.concurrent.Topic
import monocle.function.{all => Optics}
import monocle.macros.Lenses
import monocle.std.{all => StdOptics}
import org.http4s.server.websocket._

import jjm.io.FileUtil

/** The server state for a debate room.
  *
  * @param debate
  *   the full contents of a debate and current set of participants on the page.
  * @param channel
  *   the channel on which to send debate state updates to all participants
  */
@Lenses
case class DebateRoom(debate: DebateState, channel: Topic[IO, DebateState]) {
  def pushUpdate = channel.publish1(debate)
}
object DebateRoom {
  def create(debate: DebateState)(implicit c: Concurrent[IO]) = Topic[IO, DebateState](debate)
    .map(DebateRoom(debate, _))

  /** Order:
    * 1) new uninitialized rooms,
    * 2) latest turns taken
    */
  implicit val debateRoomOrder: Order[DebateRoom] = Order.by(room =>
    room
      .debate
      .debate
      .rounds
      .view
      .flatMap(_.timestamp(room.debate.debate.setup.numDebaters))
      .lastOption
      .fold(-room.debate.debate.setup.startTime)(-_)
  )
}

case class DebateStateManager(
  initializeDebate: DebateSetupSpec => IO[DebateSetup],
  rooms: Ref[IO, Map[String, DebateRoom]],
  saveDir: NIOPath,
  pushUpdateRef: Ref[IO, IO[Unit]]
)(implicit c: Concurrent[IO]) {

  private def roomMembersL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)
    .composeLens(DebateState.participants)

  private def roomStateL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)

  def getRoomList = rooms
    .get
    .map {
      _.toVector
        .sortBy(_._2)
        .map { case (roomName, room) =>
          RoomMetadata(
            name = roomName,
            storyTitle = room.debate.debate.setup.sourceMaterial.title,
            roleAssignments = room.debate.debate.setup.roles,
            creationTime = room.debate.debate.setup.startTime,
            status = room.debate.status,
            latestUpdateTime = room
              .debate
              .debate
              .rounds
              .view
              .flatMap(_.timestamp(room.debate.debate.setup.numDebaters))
              .lastOption
              .getOrElse(room.debate.debate.setup.startTime),
            result = room.debate.debate.result,
            whoseTurnIsNext = room
              .debate
              .debate
              .currentTransitions
              .toOption
              .foldMap(_.currentSpeakers),
            currentParticipants = room.debate.participants.map(_.name)
          )
        }
    }

  def pushUpdate = pushUpdateRef.get.flatten

  def deleteDebate(roomName: String) =
    for {
      _ <- rooms.update(_ - roomName)
      _ <- IO(Files.delete(saveDir.resolve(roomName + ".json")))
      _ <- pushUpdate
    } yield ()

  def addParticipant(roomName: String, participantId: String) =
    for {
      _ <- rooms.update(
        roomStateL(roomName).modify { debateState =>
          val role = debateState
            .debate
            .setup
            .roles
            .find(_._2 == participantId)
            .map(_._1)
            .getOrElse(Observer)
          debateState.addParticipant(ParticipantId(participantId, role))
        }
      )
      _ <- rooms.get.flatMap(_.get(roomName).traverse_(_.pushUpdate))
      _ <- pushUpdate
    } yield ()

  def removeParticipant(roomName: String, participantId: String) =
    for {
      _ <- rooms.update(roomMembersL(roomName).modify(_.filter(_.name != participantId)))
      _ <- rooms.get.map(_.get(roomName).traverse_(_.pushUpdate))
      _ <- pushUpdate
    } yield ()

  def createDebate(roomName: String, setupSpec: DebateSetupSpec) =
    for {
      setup <- initializeDebate(setupSpec)
      debate = Debate(setup, Vector())
      room     <- DebateRoom.create(DebateState(debate = debate, participants = Set()))
      curRooms <- rooms.get
      _ <-
        curRooms.get(roomName) match {
          case Some(_) =>
            IO.unit // do nothing
          case None =>
            rooms.update(_ + (roomName -> room)) >>
              FileUtil.writeJson(saveDir.resolve(roomName + ".json"))(debate)
        }
      _ <- pushUpdate
    } yield ()

  def processUpdate(roomName: String, request: DebateStateUpdateRequest) = {
    val updateState =
      request match {
        case DebateStateUpdateRequest.State(debateState) =>
          rooms.update(roomStateL(roomName).set(debateState)) // update state
      }

    for {
      _           <- updateState
      debateState <- rooms.get.map(_.apply(roomName).debate)
      _ <-
        FileUtil.writeJson(saveDir.resolve(roomName + ".json"))(
          debateState.debate
        ) // save after every change
      // TODO: maybe update clients on the new room list since room order has changed? Or unnecessary computation?
      // _ <- getRoomList.flatMap(mainChannel.publish1) // update all clients on the new room list
    } yield debateState

  }

  def createWebsocket(roomName: String, participantName: String)(implicit timer: Timer[IO]) =
    for {
      _    <- addParticipant(roomName, participantName)
      room <- rooms.get.map(_.apply(roomName))
      outStream = Stream
        .emit[IO, Option[DebateState]](Some(room.debate))
        .merge(room.channel.subscribe(100).map(Some(_)))
        .merge(Stream.awakeEvery[IO](30.seconds).map(_ => None))
        .map(pickleToWSFrame(_))
        .through(filterCloseFrames)
      res <- WebSocketBuilder[IO].build(
        send = outStream,
        receive =
          x =>
            room
              .channel
              .publish(
                filterCloseFrames(x)
                  .map(unpickleFromWSFrame[DebateStateUpdateRequest])
                  .evalMap(processUpdate(roomName, _))
              ),
        onClose = removeParticipant(roomName, participantName)
      )
    } yield res

  def getLeaderboard = rooms
    .get
    .map { roomMap =>
      Leaderboard.fromDebates(roomMap.values.toList.map(_.debate.debate))
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
      _     <- IO(os.makeDir.all(saveDirOs))
      files <- IO(os.list(saveDirOs).map(_.toNIO).filter(_.toString.endsWith(".json")).toVector)
      rooms <- files
        .traverse(path =>
          FileUtil
            .readJson[Debate](path)
            .flatMap(debate => DebateRoom.create(DebateState(debate, Set())))
            .map { room =>
              val roomName = path.getFileName.toString.dropRight(".json".length)
              roomName -> room
            }
        )
        .map(_.toMap)
      roomsRef <- Ref[IO].of(rooms)
    } yield DebateStateManager(initializeDebate, roomsRef, saveDir, pushUpdateRef)
  }
}
