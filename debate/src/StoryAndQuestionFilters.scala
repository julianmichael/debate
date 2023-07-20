package debate

import debate.service.QuALITYStoryMetadata
import monocle.macros.Lenses
import io.circe.generic.JsonCodec
import debate.quality.QuALITYQuestion

import jjm.implicits._

@Lenses
@JsonCodec
case class StoryAndQuestionFilters(
  gutenberg: Boolean = true,
  nonGutenberg: Boolean = false,
  debatedStories: Boolean = false,
  nonDebatedStories: Boolean = true,
  storiesWithOverlap: Boolean = true,
  storiesWithNoOverlap: Boolean = false,
  overlappingQuestions: Boolean = true,
  nonOverlappingQuestions: Boolean = false,
  debatedQuestions: Boolean = false,
  nonDebatedQuestions: Boolean = true,
  noLabels: Boolean = false,
  writerLabelAgreesWithGoldLabel: Boolean = true,
  writerLabelDoesntAgreeWithGoldLabel: Boolean = false,
  minUntimedAccuracyAgainstGold: Double = 1.0,
  maxSpeedAccuracyAgainstGold: Double = 0.5,
  minAverageContextRequiredJudgment: Double = 3.0
) {
  def admitsStory(metadata: QuALITYStoryMetadata): Boolean = {
    val overlap =
      if (metadata.numSingleTurnDebateMatches > 0)
        storiesWithOverlap
      else
        storiesWithNoOverlap
    val debated =
      if (metadata.hasBeenDebated)
        debatedStories
      else
        nonDebatedStories
    val source =
      if (metadata.source == "Gutenberg")
        gutenberg
      else
        nonGutenberg
    overlap && debated && source
  }

  def admitsQuestion(
    question: QuALITYQuestion,
    matches: Set[String],
    questionsDebated: Set[String]
  ): Boolean = {
    val overlapIsOk =
      if (matches.contains(question.questionUniqueId))
        overlappingQuestions
      else
        nonOverlappingQuestions

    val debatedIsOk =
      if (questionsDebated.contains(question.question)) {
        debatedQuestions
      } else
        nonDebatedQuestions

    overlapIsOk && debatedIsOk &&
    question
      .annotations
      .fold(noLabels) { annotations =>
        val labelAgr =
          if (annotations.goldLabel == annotations.writerLabel)
            writerLabelAgreesWithGoldLabel
          else
            writerLabelDoesntAgreeWithGoldLabel

        labelAgr && annotations.untimedAccuracyAgainstGold >= minUntimedAccuracyAgainstGold &&
        annotations.speedAccuracyAgainstGold <= maxSpeedAccuracyAgainstGold &&
        annotations.context.meanOpt.forall(_ >= minAverageContextRequiredJudgment)
      }
  }
}
object StoryAndQuestionFilters
