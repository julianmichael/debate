/** Cross-platform code used by both the JVM and JS packages.
  */

import cats.Monad
import cats.Reducible
import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.Lens
import monocle.macros.Lenses

import jjm.ling.ESpan

package object debate extends PackagePlatformExtensions {

  @JsonCodec sealed trait DebateStateUpdateRequest
  object DebateStateUpdateRequest {
    case class State(state: DebateState) extends DebateStateUpdateRequest
    case class SetupSpec(spec: DebateSetupSpec) extends DebateStateUpdateRequest
  }

  val qualityServiceApiEndpoint = "quality"

  @JsonCodec sealed trait MainChannelRequest
  case class RegisterDebater(debaterName: String) extends MainChannelRequest
  case class RemoveDebater(debaterName: String) extends MainChannelRequest
  case class DeleteRoom(isOfficial: Boolean, roomName: String)
      extends MainChannelRequest

  @JsonCodec sealed trait RoomStatus {
    import RoomStatus._
    override def toString = this match {
      case SettingUp  => "setting up"
      case InProgress => "in progress"
      case Complete   => "complete"
    }

    def isComplete = this match {
      case Complete => true
      case _        => false
    }
  }
  object RoomStatus {
    case object SettingUp extends RoomStatus
    case object InProgress extends RoomStatus
    case object Complete extends RoomStatus
  }

  @Lenses @JsonCodec case class RoomMetadata(
      name: String,
      assignedParticipants: Set[String],
      currentParticipants: Set[String],
      // latestUpdateTime: Long, // TODO
      status: RoomStatus
  )

  def makePageTitle(x: String) = {
    (if (x.isEmpty) "" else s"$x | ") + "Debate"
  }
  def answerLetter(index: Int) = ('A' + index).toChar.toString

  def vectorEnd[A]: Lens[Vector[A], Option[A]] =
    Lens.apply[Vector[A], Option[A]](_ => None)(opt =>
      vec => opt.fold(vec.init)(vec :+ _)
    )

  def span2text(span: ESpan): String = s"<<${span.begin}-${span.endExclusive}>>"

  implicit class RichReducible[F[_]: Reducible, A](fa: F[A]) {
    // version of reduceLeftM which takes advantage of Monad.pure
    def reduceLeftMonadic[G[_]: Monad](g: (A, A) => G[A]): G[A] = {
      fa.reduceLeftTo(Monad[G].pure)((ga, a) => Monad[G].flatMap(ga)(g(_, a)))
    }
  }
}
