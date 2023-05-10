package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

// map from section to question to yes/no votes
@JsonCodec
@Lenses
case class OpenEndedFeedbackAnswer(answer: String, responses: Map[String, Boolean])
object OpenEndedFeedbackAnswer

@JsonCodec
@Lenses
case class OpenEndedFeedbackQuestion(question: String, answers: Vector[OpenEndedFeedbackAnswer])
object OpenEndedFeedbackQuestion

@JsonCodec
@Lenses
case class OpenEndedFeedbackSection(name: String, questions: Vector[OpenEndedFeedbackQuestion])
object OpenEndedFeedbackSection

@JsonCodec
@Lenses
case class OpenEndedFeedback(sections: Vector[OpenEndedFeedbackSection])
object OpenEndedFeedback
