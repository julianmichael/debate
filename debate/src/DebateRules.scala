package debate

import monocle.macros.Lenses
import io.circe.generic.JsonCodec
import monocle.macros.GenPrism

/** Rules of the debate. I expect to want to add more stuff here as we want to
  * vary the debates on different dimensions.
  *
  * @param fixedOpening
  *   the structure of the opening rounds of the debate
  * @param repeatingStructure
  *   the structure to repeat indefinitely after the opening rounds
  * @param scoringFunction
  *   the scoring function to used in reward calculations
  */
@Lenses @JsonCodec case class DebateRules(
    fixedOpening: Vector[DebateRoundType],
    repeatingStructure: Vector[DebateRoundType],
    scoringFunction: ScoringFunction
) {

  def roundTypes: LazyList[DebateRoundType] = {
    // TODO change return type to some kind of infinite lazy list, it should always be that
    LazyList.from(fixedOpening) #:::
      LazyList.continually(repeatingStructure).flatten
  }

}
object DebateRules {

  /** Default rules. */
  def default: DebateRules = DebateRules(
    Vector(
      DebateRoundType.JudgeFeedbackRound(true, 500),
      DebateRoundType.SimultaneousSpeechesRound(500),
      DebateRoundType.JudgeFeedbackRound(true, 500)
    ),
    Vector(
      DebateRoundType.SequentialSpeechesRound(500),
      DebateRoundType.JudgeFeedbackRound(true, 500)
    ),
    ScoringFunction.LogScoreWithLinearPenalty.default
  )
}

sealed trait DebateTurnTypeResult extends Product with Serializable
object DebateTurnTypeResult {
  case class Turn(turn: DebateTurnType) extends DebateTurnTypeResult
  case object Next extends DebateTurnTypeResult
  case class End(finalJudgement: Vector[Double]) extends DebateTurnTypeResult
  case object Mismatch extends DebateTurnTypeResult
}

/** Schema for round types used to set up the debate. */
@JsonCodec sealed trait DebateRoundType {
  import DebateRoundType._
  def getFirstTurn(numDebaters: Int): DebateTurnType = {
    require(numDebaters > 0)
    this match {
      case SimultaneousSpeechesRound(charLimit) =>
        DebateTurnType.SimultaneousSpeechesTurn(
          (0 until numDebaters).toSet,
          charLimit
        )
      case SequentialSpeechesRound(charLimit) =>
        DebateTurnType.DebaterSpeechTurn(0, charLimit)
      case JudgeFeedbackRound(reportBeliefs, charLimit) =>
        DebateTurnType.JudgeFeedbackTurn(reportBeliefs, charLimit)
    }
  }

  // returns None if round type and round are incompatible
  def getTurn(round: DebateRound, numDebaters: Int): DebateTurnTypeResult =
    (this, round) match {
      case (
            SimultaneousSpeechesRound(charLimit),
            SimultaneousSpeeches(speeches)
          ) =>
        if (speeches.size == numDebaters) DebateTurnTypeResult.Next
        else {
          DebateTurnTypeResult.Turn(
            DebateTurnType.SimultaneousSpeechesTurn(
              (0 until numDebaters).toSet -- speeches.keySet,
              charLimit
            )
          )
        }
      case (SequentialSpeechesRound(charLimit), SequentialSpeeches(speeches)) =>
        val remainingDebaters = (0 until numDebaters).toSet -- speeches.keySet
        remainingDebaters.minOption.fold(
          DebateTurnTypeResult.Next: DebateTurnTypeResult
        ) { nextDebater =>
          DebateTurnTypeResult.Turn(
            DebateTurnType.DebaterSpeechTurn(nextDebater, charLimit)
          )
        }
      case (JudgeFeedbackRound(_, _), JudgeFeedback(judgment, _, endDebate)) =>
        if (!endDebate) DebateTurnTypeResult.Next
        else DebateTurnTypeResult.End(judgment)
      case _ => DebateTurnTypeResult.Mismatch
    }
}
object DebateRoundType {
  @Lenses @JsonCodec case class SimultaneousSpeechesRound(charLimit: Int)
      extends DebateRoundType
  val simultaneousSpeechesRound =
    GenPrism[DebateRoundType, SimultaneousSpeechesRound]
  @Lenses @JsonCodec case class SequentialSpeechesRound(charLimit: Int)
      extends DebateRoundType
  val sequentialSpeechesRound =
    GenPrism[DebateRoundType, SequentialSpeechesRound]
  @Lenses @JsonCodec case class JudgeFeedbackRound(
      reportBeliefs: Boolean,
      charLimit: Int
  ) extends DebateRoundType
  val judgeFeedbackRound = GenPrism[DebateRoundType, JudgeFeedbackRound]
}
