package debate

import cats.data.NonEmptySet

import io.circe.generic.JsonCodec

@JsonCodec
sealed trait MainChannelRequest
case class Poke(roomName: String, pokees: NonEmptySet[String]) extends MainChannelRequest
case class RegisterRuleConfig(ruleConfig: RuleConfig)          extends MainChannelRequest
case class RemoveRuleConfig(ruleConfigName: String)            extends MainChannelRequest
case class RegisterDebater(profile: Profile)                   extends MainChannelRequest
case class RemoveDebater(debaterName: String)                  extends MainChannelRequest
case class RefreshLeaderboard()                                extends MainChannelRequest
case class CreateRoom(isOfficial: Boolean, roomName: String, setupSpec: DebateSetupSpec)
    extends MainChannelRequest
case class CreateRooms(isOfficial: Boolean, setups: Vector[DebateSetup]) extends MainChannelRequest
case class ScheduleOfflineJudges(isOfficial: Boolean, assignments: Vector[(String, String)])
    extends MainChannelRequest
case class ExecutePendingTurns(name: String) extends MainChannelRequest

case class DeleteRoom(isOfficial: Boolean, roomName: String) extends MainChannelRequest
