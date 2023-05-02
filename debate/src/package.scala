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
import cats.Monoid
import monocle.Iso
import jjm.ling.Span
import cats.Order
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import scala.collection.immutable.SortedSet

package object debate extends PackagePlatformExtensions {

  def tapPrint[A](a: A): A = {
    println(a)
    a
  }

  val adminUsername = "Admin"

  // you're only allowed to judge the same story once
  val numJudgingsAllowedPerStory = 1

  val timeBeforeWhichToIgnoreMissingFeedback = 1672531200000L

  val appDivId = "app"

  type Constant[C, A] = C

  def optionIsoWithEmpty[A: Monoid] =
    Iso[Option[A], A](_.combineAll)(a =>
      if (a == Monoid[A].empty)
        None
      else
        Some(a)
    )

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
    timestamp: Long,
    correctAnswerIndex: Int,
    endedBy: DebateEndReason,
    judgingInfo: Option[JudgingResult]
  )
  object DebateResult

  @JsonCodec
  sealed trait OfflineJudgingInfo {
    def judgment: Vector[Double]
    def explanation: String
    def timestamp: Long
  }
  object OfflineJudgingInfo {
    case class Timed(
      val judgment: Vector[Double],
      val explanation: String,
      val timestamp: Long,
      timeTakenMillis: Long
    ) extends OfflineJudgingInfo
    case class Stepped(
      val judgment: Vector[Double],
      val explanation: String,
      val timestamp: Long,
      numContinues: Int
    ) // extends OfflineJudgingInfo
  }

  @JsonCodec
  sealed trait DebateStateUpdateRequest
  object DebateStateUpdateRequest {
    case class State(state: DebateState) extends DebateStateUpdateRequest
  }

  val ajaxServiceApiEndpoint      = "api"
  val analyticsServiceApiEndpoint = "analytics"
  val qualityServiceApiEndpoint   = "quality"

  def makePageTitle(x: String) =
    Option(x.trim).filter(_.nonEmpty).map(_ + " | ").combineAll + "Debate"

  def answerLetter(index: Int) = ('A' + index).toChar.toString

  def vectorEnd[A]: Lens[Vector[A], Option[A]] =
    Lens.apply[Vector[A], Option[A]](_ => None)(opt => vec => opt.fold(vec.init)(vec :+ _))

  def span2text(span: ESpan): String = s"<<${span.begin}-${span.endExclusive}>>"

  def itemMatchesKeywordQuery(itemTerms: Set[String], queryKeywords: Set[String]) = queryKeywords
    .forall { qk =>
      val k = qk.toLowerCase
      itemTerms.exists(_.toLowerCase.contains(k))
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

  implicit class RichBoolean(a: Boolean) {
    def -->(b: => Boolean) = !a || b
  }

  implicit class RichSet[A](as: Set[A]) {
    def toNes(implicit o: Order[A]): Option[NonEmptySet[A]] = NonEmptySet.fromSet(as.to(SortedSet))
  }

  implicit class RichVector[A](as: Vector[A]) {
    def toNev: Option[NonEmptyVector[A]] = NonEmptyVector.fromVector(as)
  }

  implicit class RichUnorderedFoldable[F[_]: UnorderedFoldable, A](fa: F[A]) {
    def existsAs(p: PartialFunction[A, Boolean]): Boolean =
      fa.unorderedFoldMap(a => Eval.later(p.lift(a).getOrElse(false)))(orEvalMonoid).value

    def counts_ : Map[A, Int] = fa.unorderedFoldMap(a => Map(a -> 1))
  }

  implicit class RichReducible[F[_]: Reducible, A](fa: F[A]) {
    // version of reduceLeftM which takes advantage of Monad.pure
    def reduceLeftMonadic[G[_]: Monad](g: (A, A) => G[A]): G[A] =
      fa.reduceLeftTo(Monad[G].pure)((ga, a) => Monad[G].flatMap(ga)(g(_, a)))

    def mean(implicit N: Numeric[A]): Double = {
      val (sum, count) = fa.reduceMap(x => (N.toDouble(x), 1))
      sum / count
    }
  }

  implicit class RichESpan(span: ESpan) {
    def contains(other: Span) = span.begin <= other.begin && span.endExclusive >= other.endExclusive
    def +(offset: Int)        = span.translate(offset)
    def -(offset: Int)        = span.translate(-offset)
  }
}
