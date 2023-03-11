package debate

import io.circe.generic.JsonCodec
import cats.data.NonEmptySet

@JsonCodec
sealed trait MainChannelRequest
case class Poke(roomName: String, pokees: NonEmptySet[String]) extends MainChannelRequest
case class RegisterRuleConfig(ruleConfig: RuleConfig)          extends MainChannelRequest
case class RegisterDebater(profile: Profile)                   extends MainChannelRequest
case class RemoveDebater(debaterName: String)                  extends MainChannelRequest
case class CreateRoom(isOfficial: Boolean, roomName: String, setupSpec: DebateSetupSpec)
    extends MainChannelRequest
case class DeleteRoom(isOfficial: Boolean, roomName: String) extends MainChannelRequest
