package debate

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import cats.kernel.Order

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
  object Human {
    implicit val humanProfileOrder = Order.by[Human, String](_.name)
  }
  @JsonCodec
  case class AI(val name: String, localPort: Option[Int]) extends Profile
  object AI {
    implicit val aiProfileOrder = Order.by[AI, String](_.name)
  }

  val human = GenPrism[Profile, Human]
  val ai    = GenPrism[Profile, AI]

  implicit val profileOrder = Order
    .whenEqual(Order.by[Profile, Boolean](_.isHuman), Order.by[Profile, String](_.name))
}
