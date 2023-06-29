package debate

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism

@JsonCodec
sealed trait Profile {
  def name: String
  def isHuman: Boolean =
    this match {
      case Profile.Human(_, _) =>
        true
      case _ =>
        false
    }
  def isAI: Boolean =
    this match {
      case Profile.AI(_, _) =>
        true
      case _ =>
        false
    }
}
object Profile {
  @JsonCodec
  case class Human(val name: String, slackEmail: Option[String]) extends Profile
  @JsonCodec
  case class AI(val name: String, localPort: Option[Int]) extends Profile

  val human = GenPrism[Profile, Human]
  val ai    = GenPrism[Profile, AI]
}
