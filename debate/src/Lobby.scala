package debate

import cats.implicits._
import io.circe.generic.JsonCodec
import monocle.macros.Lenses
import debate.RoomStatus.WaitingToBegin
import cats.kernel.CommutativeMonoid

sealed trait DebateProgressLabel
object DebateProgressLabel {
  case object Assigned         extends DebateProgressLabel
  case object Begun            extends DebateProgressLabel
  case object AwaitingFeedback extends DebateProgressLabel
  case object Complete         extends DebateProgressLabel
}

case class DebaterStoryStats(
  debating: Map[DebateProgressLabel, Int] = Map(),
  liveJudging: Map[DebateProgressLabel, Int] = Map(),
  offlineJudging: Map[DebateProgressLabel, Int] = Map()
)
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
                case WaitingToBegin =>
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
                DebaterStoryStats(debating = Map(label -> 1))
              } else
                DebaterStoryStats(liveJudging = Map(label -> 1))

            debater -> Map(room.sourceMaterialId -> stats)
          }
          .toMap

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
              val stats = DebaterStoryStats(offlineJudging = Map(label -> 1))
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
