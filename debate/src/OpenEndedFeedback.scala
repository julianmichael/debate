package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

// map from section to question to yes/no votes
@JsonCodec
@Lenses
case class OpenEndedFeedbackQuestion(question: String, responses: Map[String, Boolean])
object OpenEndedFeedbackQuestion

@JsonCodec
@Lenses
case class OpenEndedFeedbackSection(name: String, questions: Vector[OpenEndedFeedbackQuestion])
object OpenEndedFeedbackSection

@JsonCodec
@Lenses
case class OpenEndedFeedback(sections: Vector[OpenEndedFeedbackSection])
object OpenEndedFeedback
