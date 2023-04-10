package debate

import cats.kernel.Order

import io.circe.KeyDecoder
import io.circe.KeyEncoder
import io.circe.generic.JsonCodec
import monocle.macros.GenPrism

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
      case d @ Debater(_) =>
        Some(d)
      case Judge =>
        Some(Judge)
      case OfflineJudge =>
        Some(OfflineJudge)
      case _ =>
        None
    }

  def asLiveDebateRoleOpt: Option[LiveDebateRole] =
    this match {
      case d @ Debater(i) =>
        Some(d)
      case Judge =>
        Some(Judge)
      case _ =>
        None
    }

  def isDebater =
    this match {
      case Debater(_) =>
        true
      case _ =>
        false
    }

  def canSeeDebaterNames: Boolean = this == Facilitator

  def canSeeWhatDebatersSee =
    this match {
      case Facilitator | Debater(_) =>
        true
      case _ =>
        false
    }

  def canSeeWhatDebaterSees(index: Int): Boolean =
    this match {
      case Facilitator | Debater(`index`) =>
        true
      case _ =>
        false
    }

  def canSeeStory: Boolean                 = canSeeWhatDebatersSee
  def canSeeVotes: Boolean                 = canSeeWhatDebatersSee
  def canSeeIntermediateArguments: Boolean = canSeeWhatDebatersSee

  def seesRoundTimestamp: Boolean = !canSeeWhatDebatersSee
}
case object Observer extends Role {
  override def toString = "Observer"
}
case object Facilitator extends Role {
  override def toString = "Facilitator"
}
// peeper: someone trying to look at a debate they don't have permission to
case object Peeper extends Role {
  override def toString = "Peeper"
}
@JsonCodec
sealed trait DebateRole extends Role
@JsonCodec
sealed trait JudgeRole extends DebateRole
case object OfflineJudge extends DebateRole with JudgeRole {
  override def toString = "Offline Judge (Timed)"
}
@JsonCodec
sealed trait LiveDebateRole extends DebateRole
@JsonCodec
case class Debater(answerIndex: Int) extends LiveDebateRole {
  override def toString = s"Debater ${answerLetter(answerIndex)}"
}
case object Judge extends LiveDebateRole with JudgeRole {
  override def toString = "Judge"
}
object LiveDebateRole {

  def allRoles(numDebaters: Int): List[LiveDebateRole] =
    Judge :: (0 until numDebaters).map(Debater(_)).toList

  object DebaterIndex {
    def unapply(x: String) =
      if (x.length == 1 && x.charAt(0).isLetter) {
        Some(x.charAt(0) - 'A')
      } else
        None
  }

  def fromString(x: String): Option[LiveDebateRole] =
    x match {
      case "Judge" =>
        Some(Judge)
      case s"Debater ${DebaterIndex(index)}" =>
        Some(Debater(index))
      case _ =>
        None
    }

  implicit val liveDebateRoleKeyEncoder = KeyEncoder.instance[LiveDebateRole](_.toString)
  implicit val liveDebateRoleKeyDecoder = KeyDecoder.instance[LiveDebateRole](fromString)
  implicit val liveDebateRoleOrder = Order.by[LiveDebateRole, Int] {
    case Judge =>
      -1
    case Debater(i) =>
      i
  }
}
object DebateRole {
  implicit val debateRoleOrder = Order.by[DebateRole, Int] {
    case Judge =>
      -2
    case OfflineJudge =>
      -1
    case Debater(i) =>
      i
  }

  def fromString(x: String): Option[DebateRole] =
    x match {
      case "Offline Judge" =>
        Some(OfflineJudge)
      case x =>
        LiveDebateRole.fromString(x)
    }
  implicit val debateRoleKeyEncoder = KeyEncoder.instance[DebateRole](_.toString)
  implicit val debateRoleKeyDecoder = KeyDecoder.instance[DebateRole](fromString)
}
object Role {
  def debateRole     = GenPrism[Role, DebateRole]
  def liveDebateRole = GenPrism[Role, LiveDebateRole]

  def fromString(x: String): Option[Role] =
    x match {
      case "Observer" =>
        Some(Observer)
      case "Facilitator" =>
        Some(Facilitator)
      case "Peeper" =>
        Some(Peeper)
      case x =>
        DebateRole.fromString(x)
    }
  implicit val roleKeyEncoder = KeyEncoder.instance[Role](_.toString)
  implicit val roleKeyDecoder = KeyDecoder.instance[Role](fromString)
}
