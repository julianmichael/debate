package debate

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import monocle.macros.Lenses


@JsonCodec sealed trait SourceMaterialSpec
@Lenses @JsonCodec case class CustomSourceMaterialSpec(title: String, contents: String) extends SourceMaterialSpec
object CustomSourceMaterialSpec
@Lenses @JsonCodec case class QuALITYSourceMaterialSpec(articleId: String) extends SourceMaterialSpec
object QuALITYSourceMaterialSpec
object SourceMaterialSpec {
  val custom = GenPrism[SourceMaterialSpec, CustomSourceMaterialSpec]
  val quality = GenPrism[SourceMaterialSpec, QuALITYSourceMaterialSpec]
}

@Lenses @JsonCodec case class DebateSetupSpec(
    rules: DebateRules,
    sourceMaterial: SourceMaterialSpec,
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
    sourceMaterial = CustomSourceMaterialSpec("Title", "Source material."),
    question = "Question?",
    answers = Vector("Answer 1", "Answer 2"),
    roles = Map(),
    correctAnswerIndex = 0
  )
}
