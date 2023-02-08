package debate

import cats.implicits._
import io.circe.generic.JsonCodec
import monocle.macros.Lenses
import cats.kernel.CommutativeMonoid

case class DebaterStoryStats(
  debating: Map[DebateProgressLabel, Set[String]] = Map(),
  liveJudging: Map[DebateProgressLabel, Set[String]] = Map(),
  offlineJudging: Map[DebateProgressLabel, Set[String]] = Map()
) {
  def allJudging = liveJudging |+| offlineJudging
  import DebateProgressLabel._
  def hasReadStory      = (debating - Assigned).nonEmpty
  def needsToJudgeStory = (allJudging - Complete - AwaitingFeedback).nonEmpty
}
object DebaterStoryStats {
  implicit val debaterStoryStatsCommutativeMonoid: CommutativeMonoid[DebaterStoryStats] =
    cats.derived.semiauto.commutativeMonoid
}

@Lenses
@JsonCodec
case class Lobby(
  allDebaters: Set[String],
  trackedDebaters: Set[String],
  officialRooms: Set[RoomMetadata],
  practiceRooms: Set[RoomMetadata],
  leaderboard: Leaderboard
) {
  lazy val storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]] = officialRooms
    .unorderedFoldMap { room =>
      val live =
        room
          .roleAssignments
          .view
          .map { case (role, debater) =>
            val label =
              room.status match {
                case RoomStatus.WaitingToBegin =>
                  DebateProgressLabel.Assigned
                case RoomStatus.InProgress =>
                  if (room.peopleWhoHaveSpoken.contains(debater)) {
                    DebateProgressLabel.Begun
                  } else
                    DebateProgressLabel.Assigned
                case RoomStatus.Complete(_, _, feedbackProviders) =>
                  if (feedbackProviders.contains(debater))
                    DebateProgressLabel.Complete
                  else
                    DebateProgressLabel.AwaitingFeedback
              }
            val stats =
              if (role == Judge) {
                DebaterStoryStats(liveJudging = Map(label -> Set(room.name)))
              } else
                DebaterStoryStats(debating = Map(label -> Set(room.name)))

            Map(debater -> Map(room.sourceMaterialId -> stats))
          }
          .toVector
          .combineAll

      // TODO: add assignments for offline judging
      val offline = RoomStatus
        .complete
        .getOption(room.status)
        .foldMap { case RoomStatus.Complete(_, offlineJudgingResults, feedbackProviders) =>
          offlineJudgingResults
            .keySet
            .view
            .map { judge =>
              val label =
                if (feedbackProviders.contains(judge)) {
                  DebateProgressLabel.Complete
                } else
                  DebateProgressLabel.AwaitingFeedback
              val stats = DebaterStoryStats(offlineJudging = Map(label -> Set(room.name)))
              judge -> Map(room.sourceMaterialId -> stats)
            }
            .toMap
        }

      live |+| offline
    }
}
object Lobby {
  def empty = Lobby(Set(), Set(), Set(), Set(), Leaderboard(Map()))
}
