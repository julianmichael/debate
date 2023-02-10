/** Cross-platform code used by both the JVM and JS packages.
  */

import cats.Eval
import cats.Monad
import cats.Reducible
import cats.UnorderedFoldable
import cats.implicits._
import cats.kernel.CommutativeMonoid

import io.circe.generic.JsonCodec
import monocle.Lens
import monocle.macros.Lenses

import jjm.ling.ESpan

package object debate extends PackagePlatformExtensions {

  val timeBeforeWhichToIgnoreMissingFeedback = 1672531200000L

  val appDivId = "app"

  @JsonCodec
  sealed trait DebateEndReason
  object DebateEndReason {
    case object JudgeDecided    extends DebateEndReason
    case object TimeUp          extends DebateEndReason
    case object MutualAgreement extends DebateEndReason
  }

  @Lenses
  @JsonCodec
  case class JudgingResult(
    correctAnswerIndex: Int,
    numContinues: Int,
    finalJudgement: Vector[Double],
    judgeReward: Double
  )
  object JudgingResult

  @Lenses
  @JsonCodec
  case class DebateResult(
    correctAnswerIndex: Int,
    endedBy: DebateEndReason,
    judgingInfo: Option[JudgingResult]
  )
  object DebateResult

  @JsonCodec
  sealed trait OfflineJudgingResult {
    def judgment: Vector[Double]
    def explanation: String
    def timestamp: Long
  }
  object OfflineJudgingResult {
    case class Timed(
      val judgment: Vector[Double],
      val explanation: String,
      val timestamp: Long,
      timeTakenMillis: Long
    ) extends OfflineJudgingResult
    case class Stepped(
      val judgment: Vector[Double],
      val explanation: String,
      val timestamp: Long,
      numContinues: Int
    )
  }

  @JsonCodec
  sealed trait DebateStateUpdateRequest
  object DebateStateUpdateRequest {
    case class State(state: DebateState) extends DebateStateUpdateRequest
  }

  val qualityServiceApiEndpoint = "quality"

  @JsonCodec
  sealed trait MainChannelRequest
  case class RegisterDebater(debaterName: String) extends MainChannelRequest
  case class RemoveDebater(debaterName: String)   extends MainChannelRequest
  case class CreateRoom(isOfficial: Boolean, roomName: String, setupSpec: DebateSetupSpec)
      extends MainChannelRequest
  case class DeleteRoom(isOfficial: Boolean, roomName: String) extends MainChannelRequest

  def makePageTitle(x: String) =
    Option(x.trim).filter(_.nonEmpty).map(_ + " | ").combineAll + "Debate"

  def answerLetter(index: Int) = ('A' + index).toChar.toString

  def vectorEnd[A]: Lens[Vector[A], Option[A]] =
    Lens.apply[Vector[A], Option[A]](_ => None)(opt => vec => opt.fold(vec.init)(vec :+ _))

  def span2text(span: ESpan): String = s"<<${span.begin}-${span.endExclusive}>>"

  implicit class RichBoolean(a: Boolean) {
    def -->(b: => Boolean) = !a || b
  }

  private val orEvalMonoid: CommutativeMonoid[Eval[Boolean]] =
    new CommutativeMonoid[Eval[Boolean]] {
      val empty: Eval[Boolean] = Eval.False
      def combine(lx: Eval[Boolean], ly: Eval[Boolean]): Eval[Boolean] = lx.flatMap {
        case true =>
          Eval.True
        case false =>
          ly
      }
    }

  implicit class RichUnorderedFoldable[F[_]: UnorderedFoldable, A](fa: F[A]) {
    def existsAs(p: PartialFunction[A, Boolean]): Boolean =
      fa.unorderedFoldMap(a => Eval.later(p.lift(a).getOrElse(false)))(orEvalMonoid).value
  }

  implicit class RichReducible[F[_]: Reducible, A](fa: F[A]) {
    // version of reduceLeftM which takes advantage of Monad.pure
    def reduceLeftMonadic[G[_]: Monad](g: (A, A) => G[A]): G[A] =
      fa.reduceLeftTo(Monad[G].pure)((ga, a) => Monad[G].flatMap(ga)(g(_, a)))
  }

  def itemMatchesKeywordQuery(itemTerms: Set[String], queryKeywords: Set[String]) = queryKeywords
    .forall { qk =>
      val k = qk.toLowerCase
      itemTerms.exists(_.toLowerCase.contains(k))
    }
}
