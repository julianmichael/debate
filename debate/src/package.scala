/** Cross-platform code used by both the JVM and JS packages.
  */

import cats.Monad
import cats.Reducible
import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.Lens

import jjm.ling.ESpan
import cats.UnorderedFoldable
import cats.kernel.CommutativeMonoid

package object debate extends PackagePlatformExtensions {

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

  @JsonCodec
  sealed trait RoomStatus {
    import RoomStatus._
    override def toString =
      this match {
        case WaitingToBegin =>
          "waiting to begin"
        case InProgress =>
          "in progress"
        case Complete =>
          "complete"
      }

    def isComplete =
      this match {
        case Complete =>
          true
        case _ =>
          false
      }

    def titleString =
      this match {
        case WaitingToBegin =>
          "Waiting to Begin"
        case InProgress =>
          "In Progress"
        case Complete =>
          "Complete"
      }
  }
  object RoomStatus {
    case object WaitingToBegin extends RoomStatus
    case object InProgress     extends RoomStatus
    case object Complete       extends RoomStatus
  }

  def makePageTitle(x: String) =
    Option(x.trim).filter(_.nonEmpty).map(_ + " | ").combineAll + "Debate"

  def answerLetter(index: Int) = ('A' + index).toChar.toString

  def vectorEnd[A]: Lens[Vector[A], Option[A]] =
    Lens.apply[Vector[A], Option[A]](_ => None)(opt => vec => opt.fold(vec.init)(vec :+ _))

  def span2text(span: ESpan): String = s"<<${span.begin}-${span.endExclusive}>>"

  implicit class RichBoolean(a: Boolean) {
    def -->(b: => Boolean) = !a || b
  }

  implicit class RichUnorderedFoldable[F[_]: UnorderedFoldable, A](fa: F[A]) {
    // TODO: use laziness correctly here. I can't wrap my head around proper use of Eval
    def existsAs(p: PartialFunction[A, Boolean]): Boolean =
      fa.unorderedFoldMap(p)(CommutativeMonoid.instance(false, _ || _))
    // fa.foldRight(Eval.False)((a, exEval) => exEval.map(ex => p.lift(a).getOrElse(false) || ex))
    //   .value
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
