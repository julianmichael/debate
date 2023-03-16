package debate

import cats.implicits._
import cats.kernel.CommutativeMonoid

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

case class DebaterStoryStats(
  debating: Map[DebateProgressLabel, Set[String]] = Map(),
  liveJudging: Map[DebateProgressLabel, Set[String]] = Map(),
  offlineJudging: Map[DebateProgressLabel, Set[String]] = Map()
) {
  def allJudging = liveJudging |+| offlineJudging
  import DebateProgressLabel._
  def hasReadStory      = (debating - Assigned).nonEmpty
  def needsToJudgeStory = (allJudging - Complete - AwaitingFeedback).nonEmpty
  def canJudgeMore      = allJudging.unorderedFoldMap(_.size) < numJudgingsAllowedPerStory
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
  profiles: Map[String, Profile],
  allDebaters: Set[String],
  presentDebaters: Set[String],
  officialRooms: Set[RoomMetadata],
  practiceRooms: Set[RoomMetadata],
  leaderboard: Leaderboard,
  ruleConfigs: Map[String, RuleConfig]
) {
  lazy val storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]] = RoomMetadata
    .constructStoryRecord(officialRooms)
}
object Lobby {
  def empty = Lobby(Map(), Set(), Set(), Set(), Set(), Leaderboard(Map()), Map())
}
