package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import scala.concurrent.duration._

import cats.effect.Blocker
import cats.effect.Concurrent
import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.implicits._

import fs2.Stream
import fs2.concurrent.Topic
import monocle.function.{all => Optics}
import monocle.macros.Lenses
import monocle.std.{all => StdOptics}
import org.http4s.server.websocket._

import jjm.implicits._
import jjm.io.FileUtil

import debate.service.Slack

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
}

case class DebateStateManager(
  initializeDebate: DebateSetupSpec => IO[DebateSetup],
  profilesRef: Ref[IO, Map[String, Profile]],
  rooms: Ref[IO, Map[String, DebateRoom]],
  leaderboard: Ref[IO, Leaderboard],
  saveDir: NIOPath,
  pushUpdateRef: Ref[IO, IO[Unit]],
  slackClientOpt: Option[Slack.Service[IO]],
  dataSummarizer: DataSummarizer
)(implicit c: Concurrent[IO]) {

  val summariesDir = saveDir.resolve("summaries")
  def writeCSVs(blocker: Blocker)(implicit cs: ContextShift[IO]): IO[Unit] =
    fs2.io.file.createDirectories[IO](blocker, summariesDir) >>
      rooms
        .get
        .map(_.mapVals(_.debate.debate))
        .flatMap(roomsVec => dataSummarizer.writeSummaries(roomsVec, summariesDir))

  // val blockingEC = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(5))

  private def roomMembersL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)
    .composeLens(DebateState.participants)

  private def roomStateL(roomName: String) = Optics
    .at[Map[String, DebateRoom], String, Option[DebateRoom]](roomName)
    .composePrism(StdOptics.some[DebateRoom])
    .composeLens(DebateRoom.debate)

  def getRoomMetadata = rooms
    .get
    .map {
      _.toVector
        .map { case (roomName, room) =>
          room.debate.metadata(roomName)
        }
        .toSet
    }

  def pushUpdate = pushUpdateRef.get.flatten

  private[this] def getFreshFilePath(dir: NIOPath, name: String, extension: String): IO[NIOPath] = {
    val nums  = None #:: LazyList.from(2).map(Option(_))
    val paths = nums.map(_.foldMap("__" + _)).map(name + _ + extension).map(dir.resolve)
    IO(paths.filter(p => !Files.exists(p)).head)
  }

  def deleteDebate(roomName: String) =
    for {
      roomOpt <- rooms.get.map(_.get(roomName))
      trashDir = DebateStateManager.getTrashDir(saveDir)
      filePath <- getFreshFilePath(trashDir, roomName, ".json")
      _        <- roomOpt.map(_.debate.debate).traverse(FileUtil.writeJson(filePath))
      _        <- rooms.update(_ - roomName)
      _        <- IO(Files.delete(saveDir.resolve(roomName + ".json")))
      _        <- pushUpdate
    } yield ()

  def addParticipant(roomName: String, participantName: String) = rooms
    .get
    .map(roomStateL(roomName).getOption)
    .flatMap(
      _.traverse_ { room =>
        val sourceMaterialId = SourceMaterialId.fromSourceMaterial(room.debate.setup.sourceMaterial)
        for {
          relevantMetadata <- getRoomMetadata.map(_.filter(_.sourceMaterialId == sourceMaterialId))
          stats =
            RoomMetadata
              .constructStoryRecord(relevantMetadata)
              .get(participantName)
              .flatMap(_.get(sourceMaterialId))
              .combineAll
          heading = RoomHeading.infer(room.metadata(roomName), participantName, stats)
          _ <- rooms.update(
            roomStateL(roomName).modify { debateState =>
              val role =
                heading match {
                  case RoomHeading.AssignedForOfflineJudging | RoomHeading
                        .EligibleForOfflineJudging =>
                    OfflineJudge
                  case RoomHeading.MustJudgeBeforeDebating =>
                    Peeper // no peeping! Judge your stuff first!
                  case _ =>
                    debateState
                      .debate
                      .setup
                      .roles
                      .find(_._2 == participantName)
                      .map(_._1)
                      .orElse(
                        debateState
                          .debate
                          .offlineJudgingResults
                          .get(participantName)
                          .as(OfflineJudge)
                      )
                      .getOrElse(Observer)
                }
              debateState.addParticipant(participantName, role)
            }
          )
          _ <- rooms.get.flatMap(_.get(roomName).traverse_(_.pushUpdate))
          _ <- pushUpdate
        } yield ()
      }
    )

  def removeParticipant(roomName: String, participantName: String) =
    for {
      _ <- rooms.update(roomMembersL(roomName).modify(_ - participantName))
      _ <- rooms.get.map(_.get(roomName).traverse_(_.pushUpdate))
      _ <- pushUpdate
    } yield ()

  def createDebates(setups: Vector[DebateSetup]): IO[Unit] = setups.traverse_ { setup =>
    val title = setup.sourceMaterial.title
    val roomTitlePrefix = title
      .split("\\s+")
      .toList
      .take(5)
      .map(_.filter(_.isLetterOrDigit))
      .mkString("-")
      .toLowerCase()
    rooms
      .get
      .map(_.keySet)
      .flatMap { existingRooms =>
        val roomName =
          LazyList.from(0).map(i => s"$roomTitlePrefix-$i").filterNot(existingRooms.contains).head
        createDebate(roomName, setup)
      }
  }

  def scheduleOfflineJudges(assignments: Vector[(String, String)]): IO[Unit] =
    assignments.traverse_ { case (roomName, judgeName) =>
      for {
        curRooms <- rooms.get
        room <-
          curRooms
            .get(roomName)
            .fold(IO.raiseError[DebateRoom](new Exception(s"Room $roomName not found")))(IO.pure)
        newRoom =
          DebateRoom
            .debate
            .composeLens(DebateState.debate)
            .composeLens(Debate.setup)
            .composeLens(DebateSetup.offlineJudges)
            .modify(_ + (judgeName -> Some(OfflineJudgingMode.Stepped)))(room)
        _ <- rooms.update(_ + (roomName -> newRoom))
      } yield ()
    } >> pushUpdate

  def createDebate(roomName: String, setupSpec: DebateSetupSpec): IO[Unit] = initializeDebate(
    setupSpec
  ).flatMap(createDebate(roomName, _))

  def createDebate(roomName: String, setup: DebateSetup): IO[Unit] =
    for {
      debatersWhoHaveReadStory <- SourceMaterial
        .quality
        .getOption(setup.sourceMaterial)
        .map(_.articleId)
        .foldMap(articleId =>
          rooms
            .get
            .map(
              _.values
                .view
                .filter(room =>
                  SourceMaterial
                    .quality
                    .getOption(room.debate.debate.setup.sourceMaterial)
                    .exists(_.articleId == articleId)
                )
                .flatMap(_.debate.debate.setup.roles.filter(_._1.isDebater).values)
                .toSet
            )
        )
      debatersNewToStory =
        setup.roles.filter(_._1.isDebater).values.toSet -- debatersWhoHaveReadStory
      debate = Debate(setup, Vector(), Map())
      room     <- DebateRoom.create(DebateState(debate = debate, participants = Map()))
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
      _ <- slackClientOpt.traverse { slack =>
        profilesRef
          .get
          .flatMap { profiles =>
            debatersNewToStory
              .toVector
              .traverse { debater =>
                slack.sendMessage(
                  profiles,
                  debater,
                  s"You've been assigned to read the story \"${setup.sourceMaterial.title}\". Check your debates. " +
                    MotivationalQuotes.newMaterial.sample()
                )
              } >>
              debate
                .currentTransitions
                .currentSpeakers
                .toVector
                .traverse(role =>
                  role
                    .asLiveDebateRoleOpt
                    .traverse(liveRole =>
                      debate
                        .setup
                        .roles
                        .get(liveRole)
                        .traverse(debater =>
                          slack.sendMessage(
                            profiles,
                            debater,
                            s"It's your turn in the new room `$roomName`! " +
                              MotivationalQuotes.yourTurn.sample()
                          )
                        )
                    )
                )
          }
      }
    } yield ()

  def refreshLeaderboard =
    for {
      roomMap  <- rooms.get
      debaters <- profilesRef.get.map(_.keySet)
      _ <- leaderboard
        .set(Leaderboard.fromDebates(roomMap.values.toList.map(_.debate.debate), debaters))
    } yield ()

  def processUpdate(roomName: String, request: DebateStateUpdateRequest) = {
    val updateState =
      request match {
        case DebateStateUpdateRequest.State(debateState) =>
          rooms.update(roomStateL(roomName).set(debateState)) // update state
      }

    for {
      priorState  <- rooms.get.map(_.apply(roomName).debate)
      _           <- updateState
      debateState <- rooms.get.map(_.apply(roomName).debate)
      profiles    <- profilesRef.get
      _ <- IO(
        (debateState.debate.isOver && !priorState.debate.isOver) ||
          debateState.debate.offlineJudgingResults.values.flatMap(_.result).toSet !=
          priorState.debate.offlineJudgingResults.values.flatMap(_.result).toSet
      ).ifM(ifTrue = refreshLeaderboard, ifFalse = IO.unit)
      _ <- slackClientOpt.traverse_ { slack =>
        for {
          _ <- {
            val curUsersWhoseTurnItIs = debateState.debate.currentTransitions.giveSpeech.keySet
            val liveUsersToNotify =
              curUsersWhoseTurnItIs
                .flatMap(_.asLiveDebateRoleOpt)
                .flatMap(debateState.debate.setup.roles.get) -- debateState.participants.keySet
            val offlineJudgesToNotify =
              if (
                !priorState.debate.currentTransitions.giveSpeech.contains(OfflineJudge) &&
                curUsersWhoseTurnItIs.contains(OfflineJudge)
              ) {
                debateState.debate.setup.offlineJudges.keySet -- debateState.participants.keySet
              } else
                Set.empty[String]

            profilesRef
              .get
              .flatMap { profiles =>
                liveUsersToNotify
                  .toVector
                  .traverse_ { debater =>
                    slack.sendMessage(
                      profiles,
                      debater,
                      s"It's your turn in `$roomName`! " + MotivationalQuotes.yourTurn.sample()
                    )
                  } >>
                  offlineJudgesToNotify
                    .toVector
                    .traverse_ { debater =>
                      slack.sendMessage(
                        profiles,
                        debater,
                        s"You can now judge in the room `$roomName`! " +
                          MotivationalQuotes.newJudging.sample()
                      )
                    }

              }
          }
          _ <- debateState
            .debate
            .result
            .filter(_ => !priorState.debate.isOver)
            .flatMap(_.judgingInfo)
            .traverse_ { result =>
              val winners = result.finalJudgement.zipWithIndex.maximaBy(_._1).map(_._2)
              val notifyWinners =
                if (winners.size > 1) { // no clear winner
                  debateState
                    .debate
                    .setup
                    .roles
                    .toVector
                    .collect {
                      case (Debater(i), name) if winners.contains(i) =>
                        name -> result.finalJudgement(i)
                    }
                    .traverse_ { case (name, prob) =>
                      slack.sendMessage(
                        profiles,
                        name,
                        f"You tied with your opponent in room `$roomName%s` with ${prob * 100.0}%.0f%% judge confidence! " +
                          MotivationalQuotes.youveTied.sample()
                      )
                    }
                } else {
                  debateState
                    .debate
                    .setup
                    .roles
                    .toVector
                    .collect {
                      case (Debater(i), name) if winners.contains(i) =>
                        name -> result.finalJudgement(i)
                    }
                    .traverse_ { case (name, prob) =>
                      slack.sendMessage(
                        profiles,
                        name,
                        f"You won in room `$roomName` with ${prob * 100.0}%.0f%% judge confidence! " +
                          MotivationalQuotes.youveWon.sample()
                      )
                    }
                }

              val notifyLosers = debateState
                .debate
                .setup
                .roles
                .toVector
                .collect {
                  case (Debater(i), name) if !winners.contains(i) =>
                    name -> result.finalJudgement(i)
                }
                .traverse_ { case (name, prob) =>
                  slack.sendMessage(
                    profiles,
                    name,
                    f"You lost in room `$roomName` with ${prob * 100.0}%.0f%% judge confidence. " +
                      MotivationalQuotes.youveLost.sample()
                  )
                }

              notifyWinners >> notifyLosers
            }
        } yield ()
      }
      _ <-
        FileUtil.writeJson(saveDir.resolve(roomName + ".json"))(
          debateState.debate
        ) // save after every change
      _ <- pushUpdate
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
}
object DebateStateManager {

  def getTrashDir(saveDir: NIOPath) = saveDir.resolve("trash")

  def init(
    initializeDebate: DebateSetupSpec => IO[DebateSetup],
    saveDir: NIOPath,
    profilesRef: Ref[IO, Map[String, Profile]],
    pushUpdateRef: Ref[IO, IO[Unit]],
    slackClientOpt: Option[Slack.Service[IO]],
    dataSummarizer: DataSummarizer
  )(implicit c: Concurrent[IO]) = {
    val saveDirOs = os.Path(saveDir, os.pwd)
    for {
      _     <- IO(os.makeDir.all(saveDirOs))
      _     <- IO(os.makeDir.all(os.Path(getTrashDir(saveDirOs.toNIO), os.pwd)))
      files <- IO(os.list(saveDirOs).map(_.toNIO).filter(_.toString.endsWith(".json")).toVector)
      rooms <- files
        .traverse(path =>
          FileUtil
            .readJson[Debate](path)
            .map(_.clean) // do anything we need to clean debates (e.g., fixing probabilities)
            .flatMap(debate => DebateRoom.create(DebateState(debate, Map())))
            .map { room =>
              val roomName = path.getFileName.toString.dropRight(".json".length)
              roomName -> room
            }
            .onError { case _: Throwable =>
              IO(System.err.println(s"Failed to read debate at path: $path"))
            }
            .flatTap { case (roomName, room) =>
              // write cleaned debates upon startup
              FileUtil.writeJson(saveDir.resolve(roomName + ".json"))(room.debate.debate)
            }
        )
        .map(_.toMap)
      roomsRef <- Ref[IO].of(rooms)
      debaters <- profilesRef.get.map(_.keySet)
      leaderboardRef <- Ref[IO]
        .of(Leaderboard.fromDebates(rooms.values.map(_.debate.debate).toList, debaters))
    } yield DebateStateManager(
      initializeDebate,
      profilesRef,
      roomsRef,
      leaderboardRef,
      saveDir,
      pushUpdateRef,
      slackClientOpt,
      dataSummarizer
    )
  }
}
