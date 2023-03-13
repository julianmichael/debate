package debate

import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import monocle.macros.Lenses

/** Outcome of a debate turn after some/all relevant parties have submitted
  * their arguments / info
  */
@JsonCodec
sealed trait DebateRound {
  def allSpeeches: Map[Role, DebateSpeech]

  def isComplete(numDebaters: Int): Boolean
  final def timestamp(numDebaters: Int): Option[Long] =
    if (!isComplete(numDebaters))
      None
    else {
      allSpeeches.values.view.map(_.timestamp).maxOption
    }
}

@Lenses
@JsonCodec
case class SimultaneousSpeeches(
  speeches: Map[Int, DebateSpeech] // map from answer index -> statement
) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches = speeches.map { case (idx, speech) =>
    Debater(idx) -> speech
  }

}
object SimultaneousSpeeches

@Lenses
@JsonCodec
case class SequentialSpeeches(speeches: Map[Int, DebateSpeech]) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches = speeches.map { case (idx, speech) =>
    Debater(idx) -> speech
  }

}
object SequentialSpeeches

@Lenses
@JsonCodec
case class JudgeFeedback(
  distribution: Vector[Double], // probability distribution
  feedback: DebateSpeech,
  endDebate: Boolean
) extends DebateRound {
  def isComplete(numDebaters: Int) = true
  def allSpeeches                  = Map(Judge -> feedback)
}
object JudgeFeedback

@Lenses
@JsonCodec
case class NegotiateEnd(votes: Map[Int, Boolean]) extends DebateRound {
  def isComplete(numDebaters: Int) = votes.size == numDebaters
  def allSpeeches                  = Map()
}
object NegotiateEnd

@Lenses
@JsonCodec
case class OfflineJudgments(judgments: Map[String, OfflineJudgment]) extends DebateRound {
  def isComplete(numDebaters: Int) = judgments.values.forall(_.result.nonEmpty)
  def allSpeeches =
    judgments
      .toVector
      .view
      .flatMap { case (judge, judgment) =>
        judgment
          .result
          .map { case OfflineJudgingResult(_, explanation, timestamp) =>
            (OfflineJudge: Role) ->
              DebateSpeech(judge, timestamp, Vector(SpeechSegment.Text(explanation)))
          }
      }
      .toMap
}
object OfflineJudgments

object DebateRound {
  val simultaneousSpeeches = GenPrism[DebateRound, SimultaneousSpeeches]
  val sequentialSpeeches   = GenPrism[DebateRound, SequentialSpeeches]
  val judgeFeedback        = GenPrism[DebateRound, JudgeFeedback]
  val negotiateEnd         = GenPrism[DebateRound, NegotiateEnd]
  val offlineJudgments     = GenPrism[DebateRound, OfflineJudgments]
}
