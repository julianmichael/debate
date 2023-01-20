package debate

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import monocle.macros.Lenses

import debate.util.Lipsum

@JsonCodec
sealed trait SourceMaterialSpec

@Lenses
@JsonCodec
case class CustomSourceMaterialSpec(title: String, contents: String) extends SourceMaterialSpec
object CustomSourceMaterialSpec {
  def default = CustomSourceMaterialSpec(title = "Lorem Ipsum", contents = Lipsum.get)
}

@Lenses
@JsonCodec
case class QuALITYSourceMaterialSpec(articleId: String) extends SourceMaterialSpec
object QuALITYSourceMaterialSpec
object SourceMaterialSpec {
  val custom  = GenPrism[SourceMaterialSpec, CustomSourceMaterialSpec]
  val quality = GenPrism[SourceMaterialSpec, QuALITYSourceMaterialSpec]
}

@Lenses
@JsonCodec
case class DebateSetupSpec(
  rules: DebateRules,
  sourceMaterial: SourceMaterialSpec,
  question: String,
  answers: Vector[String],
  roles: Map[DebateRole, String],
  correctAnswerIndex: Int
) {
  require(correctAnswerIndex >= 0 && correctAnswerIndex < answers.size)
  def areAllRolesAssigned =
    roles.contains(Judge) && answers.indices.forall(i => roles.contains(Debater(i)))

  def correctAnswer = answers(correctAnswerIndex)
}
object DebateSetupSpec {
  def init = DebateSetupSpec(
    rules = DebateRules.default,
    sourceMaterial = CustomSourceMaterialSpec.default,
    question = "Question?",
    answers = Vector("Answer 1", "Answer 2"),
    roles = Map(),
    correctAnswerIndex = 0
  )
}
