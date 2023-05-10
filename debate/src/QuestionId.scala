package debate

import io.circe.generic.JsonCodec

@JsonCodec
case class QuestionId(story: SourceMaterialId, question: String)
object QuestionId {
  def fromDebate(debate: Debate) = QuestionId(
    SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial),
    debate.setup.question
  )
}
