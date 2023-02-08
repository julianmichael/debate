package debate

import cats.kernel.Order

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.generic.JsonCodec

/** The role someone plays in a debate.
  *   - Facilitators set things up.
  *   - Debaters argue for a particular answer.
  *   - The judge ... judges.
  *   - Observer can't do anything, just watches.
  */
@JsonCodec
sealed trait Role extends Product with Serializable {
  def asDebateRoleOpt: Option[DebateRole] =
    this match {
      case d @ Debater(i) =>
        Some(d)
      case Judge =>
        Some(Judge)
      case _ =>
        None
    }

}
case object Observer extends Role {
  override def toString = "Observer"
}
case object Facilitator extends Role {
  override def toString = "Facilitator"
}
case object TimedOfflineJudge extends Role {
  override def toString = "Offline Judge (Timed)"
}
@JsonCodec
sealed trait DebateRole extends Role
@JsonCodec
case class Debater(answerIndex: Int) extends DebateRole {
  override def toString = s"Debater ${answerLetter(answerIndex)}"
}
case object Judge extends DebateRole {
  override def toString = "Judge"
}
object DebateRole {
  object DebaterIndex {
    def unapply(x: String) =
      if (x.length == 1 && x.charAt(0).isLetter) {
        Some(x.charAt(0) - 'A')
      } else
        None
  }

  implicit val debateRoleKeyEncoder = KeyEncoder.instance[DebateRole](_.toString)
  implicit val debateRoleKeyDecoder = KeyDecoder.instance[DebateRole] {
    case "Judge" =>
      Some(Judge)
    case s"Debater ${DebaterIndex(index)}" =>
      Some(Debater(index))
    case _ =>
      None
  }
  implicit val debateRoleOrder = Order.by[DebateRole, Int] {
    case Judge =>
      -1
    case Debater(i) =>
      i
  }
}
object Role {
  implicit val roleKeyEncoder = KeyEncoder.instance[Role](_.toString)
  implicit val roleKeyDecoder = KeyDecoder.instance[Role] {
    case "Observer" =>
      Some(Observer)
    case "Facilitator" =>
      Some(Facilitator)
    case x =>
      DebateRole.debateRoleKeyDecoder(x)
  }
}
