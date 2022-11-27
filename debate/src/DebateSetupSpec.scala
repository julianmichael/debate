package debate

import monocle.macros.Lenses
import io.circe.generic.JsonCodec

@Lenses @JsonCodec case class DebateSetupSpec(
    rules: DebateRules,
    sourceMaterial: String,
    question: String,
    answers: Vector[String],
    roles: Map[DebateRole, String],
    correctAnswerIndex: Int
) {
  def areAllRolesAssigned = {
    roles.contains(Judge) && answers.indices.forall(i => roles.contains(Debater(i)))
  }
}
object DebateSetupSpec {
  def init = DebateSetupSpec(
    rules = DebateRules.default,
    sourceMaterial = "Source material.",
    question = "Question?",
    answers = Vector("Answer 1", "Answer 2"),
    roles = Map(),
    correctAnswerIndex = 0
  )
}
