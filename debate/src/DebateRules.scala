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

  /** Schematic for the turns that the debate will follow. This will be compared
    * to the actual turn sequence to decide whose turn it is. Each 'round' (eg
    * SequentialSpeechesRound) needs to be expanded to its sequence of 'turns'
    * (eg sequence of DebaterSpeechTurn).
    *
    * Must always be nonempty.
    *
    * @param numDebaters
    *   the number of different answers being argued for / debaters arguing
    */
  def turnTypes(numDebaters: Int): LazyList[DebateTurnType] = {
    // TODO change return type to some kind of infinite lazy list, it should always be that
    LazyList.from(fixedOpening.flatMap(_.getTurns(numDebaters))) #:::
      LazyList
        .continually(repeatingStructure.flatMap(_.getTurns(numDebaters)))
        .flatten
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
    ScoringFunction.LogScoreWithLinearPenalty(4, 5, 2, 0.25)
  )
}

/** Schema for round types used to set up the debate. */
@JsonCodec sealed trait DebateRoundType {
  import DebateRoundType._
  def getTurns(numDebaters: Int): Vector[DebateTurnType] = this match {
    case SimultaneousSpeechesRound(charLimit) =>
      Vector(
        DebateTurnType.SimultaneousSpeechesTurn(
          (0 until numDebaters).toSet,
          charLimit
        )
      )
    case SequentialSpeechesRound(charLimit) =>
      (0 until numDebaters)
        .map(i => DebateTurnType.DebaterSpeechTurn(i, charLimit))
        .toVector
    case JudgeFeedbackRound(reportBeliefs, charLimit) =>
      Vector(DebateTurnType.JudgeFeedbackTurn(reportBeliefs, charLimit))
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
