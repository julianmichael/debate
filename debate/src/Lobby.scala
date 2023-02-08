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
  lazy val storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]] = RoomMetadata
    .constructStoryRecord(officialRooms)
}
object Lobby {
  def empty = Lobby(Set(), Set(), Set(), Set(), Leaderboard(Map()))
}
