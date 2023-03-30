package debate

import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import monocle.macros.Lenses
import io.circe.Encoder
import io.circe.Decoder

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
  def assignedDebatersOnly: Boolean

  import DebateRoundType._
  def summary(defaultCharLimit: Option[Int]): String = {
    def clStr(charLimit: Int) =
      if (defaultCharLimit.exists(_ == charLimit))
        ""
      else
        charLimit.toString
    def aoStr(assignedOnly: Boolean) =
      if (assignedOnly)
        "a:"
      else
        ""

    this match {
      case SimultaneousSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly) =>
        val quoteLimitStr = quoteLimit.foldMap(x => s"c$x")
        s"${aoStr(assignedDebatersOnly)}${clStr(charLimit)}$quoteLimitStr"
      case SequentialSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly) =>
        val quoteLimitStr = quoteLimit.foldMap(x => s"c$x")
        s"${aoStr(assignedDebatersOnly)}seq${clStr(charLimit)}$quoteLimitStr"
      case JudgeFeedbackRound(reportBeliefs, charLimit) =>
        val reportBeliefsStr =
          if (reportBeliefs)
            "b"
          else
            ""
        s"j${clStr(charLimit)}$reportBeliefsStr"
      case NegotiateEndRound(assignedDebatersOnly) =>
        s"${aoStr(assignedDebatersOnly)}end?"
      case OfflineJudgingRound =>
        "oj"
    }
  }

  def getFirstTurn(
    numDebaters: Int,
    assignedDebaters: Set[Int],
    isLastTurn: Boolean
  ): DebateTurnType = {
    require(numDebaters > 0)
    this match {
      case SimultaneousSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly) =>
        DebateTurnType.SimultaneousSpeechesTurn(
          if (assignedDebatersOnly)
            assignedDebaters
          else
            (0 until numDebaters).toSet,
          charLimit,
          quoteLimit
        )
      case SequentialSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly) =>
        DebateTurnType.DebaterSpeechTurn(
          if (assignedDebatersOnly)
            assignedDebaters.min
          else
            0,
          charLimit,
          quoteLimit
        )
      case JudgeFeedbackRound(reportBeliefs, charLimit) =>
        DebateTurnType.JudgeFeedbackTurn(reportBeliefs, charLimit, mustEndDebate = isLastTurn)
      case NegotiateEndRound(assignedDebatersOnly) =>
        DebateTurnType.NegotiateEndTurn(
          if (assignedDebatersOnly)
            assignedDebaters
          else
            (0 until numDebaters).toSet
        )
      case OfflineJudgingRound =>
        DebateTurnType.OfflineJudgingTurn(Map())
    }
  }

  // returns None if round type and round are incompatible
  def getTurn(
    round: DebateRound,
    numDebaters: Int,
    assignedDebaters: Set[Int]
  ): DebateTurnTypeResult =
    (this, round) match {
      case (
            SimultaneousSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly),
            SimultaneousSpeeches(speeches)
          ) =>
        val desiredSpeeches =
          if (assignedDebatersOnly)
            assignedDebaters
          else
            (0 until numDebaters).toSet
        if (speeches.size == desiredSpeeches.size)
          DebateTurnTypeResult.Next
        else {
          DebateTurnTypeResult.Turn(
            DebateTurnType
              .SimultaneousSpeechesTurn(desiredSpeeches -- speeches.keySet, charLimit, quoteLimit)
          )
        }
      case (
            SequentialSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly),
            SequentialSpeeches(speeches)
          ) =>
        val desiredSpeeches =
          if (assignedDebatersOnly)
            assignedDebaters
          else
            (0 until numDebaters).toSet
        val remainingDebaters = desiredSpeeches -- speeches.keySet
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
      case (NegotiateEndRound(assignedDebatersOnly), NegotiateEnd(votes)) =>
        val desiredDebaters =
          if (assignedDebatersOnly)
            assignedDebaters
          else
            (0 until numDebaters).toSet
        if (votes.size == desiredDebaters.size) {
          if (votes.values.forall(identity))
            DebateTurnTypeResult.EndByAgreement
          else
            DebateTurnTypeResult.Next
        } else {
          DebateTurnTypeResult
            .Turn(DebateTurnType.NegotiateEndTurn(desiredDebaters -- votes.keySet))
        }
      case _ =>
        DebateTurnTypeResult.Mismatch
    }
}
object DebateRoundType {

  @Lenses
  case class SimultaneousSpeechesRound(
    charLimit: Int,
    quoteLimit: Option[Int],
    assignedDebatersOnly: Boolean
  ) extends DebateRoundType {
    def charLimitOpt = Some(charLimit)
    def hasJudge     = false
    def canEndDebate = false
  }
  object SimultaneousSpeechesRound {
    // gracefully handle the case where assignedDebatersOnly is missing
    implicit val simultaneousSpeechesRoundEncoder: Encoder[SimultaneousSpeechesRound] =
      Encoder.forProduct3("charLimit", "quoteLimit", "assignedDebatersOnly")(x =>
        (x.charLimit, x.quoteLimit, x.assignedDebatersOnly)
      )
    implicit val simultaneousSpeechesRoundDecoder: Decoder[SimultaneousSpeechesRound] =
      Decoder.forProduct3("charLimit", "quoteLimit", "assignedDebatersOnly") {
        (charLimit: Int, quoteLimit: Option[Int], assignedDebatersOnly: Option[Boolean]) =>
          SimultaneousSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly.getOrElse(false))
      }
  }
  val simultaneousSpeechesRound = GenPrism[DebateRoundType, SimultaneousSpeechesRound]

  @Lenses
  case class SequentialSpeechesRound(
    charLimit: Int,
    quoteLimit: Option[Int],
    assignedDebatersOnly: Boolean
  ) extends DebateRoundType {
    def charLimitOpt = Some(charLimit)
    def hasJudge     = false
    def canEndDebate = false
  }
  object SequentialSpeechesRound {
    implicit val sequentialSpeechesRoundEncoder: Encoder[SequentialSpeechesRound] =
      Encoder.forProduct3("charLimit", "quoteLimit", "assignedDebatersOnly")(x =>
        (x.charLimit, x.quoteLimit, x.assignedDebatersOnly)
      )
    implicit val sequentialSpeechesRoundDecoder: Decoder[SequentialSpeechesRound] =
      Decoder.forProduct3("charLimit", "quoteLimit", "assignedDebatersOnly") {
        (charLimit: Int, quoteLimit: Option[Int], assignedDebatersOnly: Option[Boolean]) =>
          SequentialSpeechesRound(charLimit, quoteLimit, assignedDebatersOnly.getOrElse(false))
      }
  }
  val sequentialSpeechesRound = GenPrism[DebateRoundType, SequentialSpeechesRound]

  @Lenses
  @JsonCodec
  case class JudgeFeedbackRound(reportBeliefs: Boolean, charLimit: Int) extends DebateRoundType {
    def charLimitOpt         = Some(charLimit)
    def hasJudge             = true
    def canEndDebate         = true
    def assignedDebatersOnly = false
  }
  val judgeFeedbackRound = GenPrism[DebateRoundType, JudgeFeedbackRound]

  @Lenses
  case class NegotiateEndRound(assignedDebatersOnly: Boolean) extends DebateRoundType {
    def charLimitOpt = None
    def hasJudge     = false
    def canEndDebate = true
  }
  object NegotiateEndRound {
    implicit val negotiateEndRoundEncoder: Encoder[NegotiateEndRound] =
      Encoder.forProduct1("assignedDebatersOnly")(x => x.assignedDebatersOnly)
    implicit val negotiateEndRoundDecoder: Decoder[NegotiateEndRound] =
      Decoder.forProduct1("assignedDebatersOnly") { (assignedDebatersOnly: Option[Boolean]) =>
        NegotiateEndRound(assignedDebatersOnly.getOrElse(false))
      }
  }
  val negotiateEnd = GenPrism[DebateRoundType, NegotiateEndRound]

  case object OfflineJudgingRound extends DebateRoundType {
    def charLimitOpt         = None
    def hasJudge             = true
    def canEndDebate         = false
    def assignedDebatersOnly = false
  }
  val offlineJudgingRound = GenPrism[DebateRoundType, OfflineJudgingRound.type]
}
