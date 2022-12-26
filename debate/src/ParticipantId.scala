package debate

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.generic.JsonCodec
import monocle.macros.Lenses

/** Identifier for a debate participant, including the role they're currently
  * playing.
  */
@Lenses
@JsonCodec
case class ParticipantId(name: String, role: Role)
object ParticipantId

/** The role someone plays in a debate.
  *   - Facilitators set things up.
  *   - Debaters argue for a particular answer.
  *   - The judge ... judges.
  *   - Observer can't do anything, just watches.
  */
@JsonCodec
sealed trait Role extends Product with Serializable
case object Observer extends Role {
  override def toString = "Observer"
}
case object Facilitator extends Role {
  override def toString = "Facilitator"
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
