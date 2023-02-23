package debate

import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import monocle.macros.Lenses

sealed trait DebateTurnTypeResult extends Product with Serializable
object DebateTurnTypeResult {
  case class Turn(turn: DebateTurnType)                 extends DebateTurnTypeResult
  case object Next                                      extends DebateTurnTypeResult
  case class EndByJudge(finalJudgement: Vector[Double]) extends DebateTurnTypeResult
  case object EndByAgreement                            extends DebateTurnTypeResult
  case object Mismatch                                  extends DebateTurnTypeResult
}

/** Schema for round types used to set up the debate. */
@JsonCodec
sealed trait DebateRoundType {
  def charLimitOpt: Option[Int]
  def hasJudge: Boolean
  def canEndDebate: Boolean

  import DebateRoundType._
  def summary(defaultCharLimit: Option[Int]): String = {
    def clStr(charLimit: Int) =
      if (defaultCharLimit.exists(_ == charLimit))
        ""
      else
        charLimit.toString

    this match {
      case SimultaneousSpeechesRound(charLimit, quoteLimit) =>
        val quoteLimitStr = quoteLimit.foldMap(x => s"c$x")
        s"sim${clStr(charLimit)}$quoteLimitStr"
      case SequentialSpeechesRound(charLimit, quoteLimit) =>
        val quoteLimitStr = quoteLimit.foldMap(x => s"c$x")
        s"seq${clStr(charLimit)}$quoteLimitStr"
      case JudgeFeedbackRound(reportBeliefs, charLimit) =>
        val reportBeliefsStr =
          if (reportBeliefs)
            "b"
          else
            ""
        s"j${clStr(charLimit)}$reportBeliefsStr"
      case NegotiateEndRound =>
        "end?"
      case OfflineJudgingRound =>
        "oj"
    }
  }

  def getFirstTurn(numDebaters: Int, isLastTurn: Boolean): DebateTurnType = {
    require(numDebaters > 0)
    this match {
      case SimultaneousSpeechesRound(charLimit, quoteLimit) =>
        DebateTurnType.SimultaneousSpeechesTurn((0 until numDebaters).toSet, charLimit, quoteLimit)
      case SequentialSpeechesRound(charLimit, quoteLimit) =>
        DebateTurnType.DebaterSpeechTurn(0, charLimit, quoteLimit)
      case JudgeFeedbackRound(reportBeliefs, charLimit) =>
        DebateTurnType.JudgeFeedbackTurn(reportBeliefs, charLimit, mustEndDebate = isLastTurn)
      case NegotiateEndRound =>
        DebateTurnType.NegotiateEndTurn((0 until numDebaters).toSet)
      case OfflineJudgingRound =>
        DebateTurnType.OfflineJudgingTurn
    }
  }

  // returns None if round type and round are incompatible
  def getTurn(round: DebateRound, numDebaters: Int): DebateTurnTypeResult =
    (this, round) match {
      case (SimultaneousSpeechesRound(charLimit, quoteLimit), SimultaneousSpeeches(speeches)) =>
        if (speeches.size == numDebaters)
          DebateTurnTypeResult.Next
        else {
          DebateTurnTypeResult.Turn(
            DebateTurnType.SimultaneousSpeechesTurn(
              (0 until numDebaters).toSet -- speeches.keySet,
              charLimit,
              quoteLimit
            )
          )
        }
      case (SequentialSpeechesRound(charLimit, quoteLimit), SequentialSpeeches(speeches)) =>
        val remainingDebaters = (0 until numDebaters).toSet -- speeches.keySet
        remainingDebaters
          .minOption
          .fold(DebateTurnTypeResult.Next: DebateTurnTypeResult) { nextDebater =>
            DebateTurnTypeResult
              .Turn(DebateTurnType.DebaterSpeechTurn(nextDebater, charLimit, quoteLimit))
          }
      case (JudgeFeedbackRound(_, _), JudgeFeedback(judgment, _, endDebate)) =>
        if (!endDebate)
          DebateTurnTypeResult.Next
        else
          DebateTurnTypeResult.EndByJudge(judgment)
      case (NegotiateEndRound, NegotiateEnd(votes)) =>
        if (votes.size == numDebaters) {
          if (votes.values.forall(identity))
            DebateTurnTypeResult.EndByAgreement
          else
            DebateTurnTypeResult.Next
        } else {
          DebateTurnTypeResult
            .Turn(DebateTurnType.NegotiateEndTurn((0 until numDebaters).toSet -- votes.keySet))
        }
      case _ =>
        DebateTurnTypeResult.Mismatch
    }
}
object DebateRoundType {

  @Lenses
  @JsonCodec
  case class SimultaneousSpeechesRound(charLimit: Int, quoteLimit: Option[Int])
      extends DebateRoundType {

    def charLimitOpt = Some(charLimit)
    def hasJudge     = false
    def canEndDebate = false
  }
  val simultaneousSpeechesRound = GenPrism[DebateRoundType, SimultaneousSpeechesRound]

  @Lenses
  @JsonCodec
  case class SequentialSpeechesRound(charLimit: Int, quoteLimit: Option[Int])
      extends DebateRoundType {

    def charLimitOpt = Some(charLimit)
    def hasJudge     = false
    def canEndDebate = false
  }
  val sequentialSpeechesRound = GenPrism[DebateRoundType, SequentialSpeechesRound]

  @Lenses
  @JsonCodec
  case class JudgeFeedbackRound(reportBeliefs: Boolean, charLimit: Int) extends DebateRoundType {
    def charLimitOpt = Some(charLimit)
    def hasJudge     = true
    def canEndDebate = true
  }
  val judgeFeedbackRound = GenPrism[DebateRoundType, JudgeFeedbackRound]

  case object NegotiateEndRound extends DebateRoundType {
    def charLimitOpt = None
    def hasJudge     = false
    def canEndDebate = true
  }
  val negotiateEnd = GenPrism[DebateRoundType, NegotiateEndRound.type]

  case object OfflineJudgingRound extends DebateRoundType {
    def charLimitOpt = None
    def hasJudge     = true
    def canEndDebate = false
  }
  val offlineJudgingRound = GenPrism[DebateRoundType, OfflineJudgingRound.type]
}
