package debate

import io.circe.generic.JsonCodec

/**
  * Identifier for a debate participant, including the role they're currently playing.
  */
@JsonCodec case class ParticipantId(name: String, role: Role)

/**
  * The role someone plays in a debate.
  * - Facilitators set things up.
  * - Debaters argue for a particular answer.
  * - The judge ... judges.
  * - Observer can't do anything, just watches.
  */
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
