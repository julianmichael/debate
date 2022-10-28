package livechat

import io.circe.generic.JsonCodec

@JsonCodec sealed trait Role
case object Observer extends Role {
  override def toString = "observer"
}
case object Facilitator extends Role {
  override def toString = "facilitator"
}
@JsonCodec case class Debater(answerIndex: Int) extends Role {
  override def toString = s"debater ${answerLetter(answerIndex)}"
}
case object Judge extends Role {
  override def toString = "judge"
}
object Role

@JsonCodec case class ParticipantId(name: String, role: Role)
