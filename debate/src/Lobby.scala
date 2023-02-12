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
  def debatesUserMustJudgeFirst(roomName: String): Set[String] =
    if (debating.values.exists(_.contains(roomName))) {
      val judging = allJudging
      judging.get(Assigned).combineAll |+| judging.get(Begun).combineAll
    } else
      Set()
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
  presentDebaters: Set[String],
  officialRooms: Set[RoomMetadata],
  practiceRooms: Set[RoomMetadata],
  leaderboard: Leaderboard
) {
  lazy val storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]] = RoomMetadata
    .constructStoryRecord(officialRooms)
}
object Lobby {
  def empty = Lobby(Set(), Set(), Set(), Set(), Set(), Leaderboard(Map()))
}
