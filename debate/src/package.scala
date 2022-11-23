/** Cross-platform code used by both the JVM and JS packages.
  */

import monocle.Lens
import monocle.macros.Lenses

import io.circe.generic.JsonCodec

import jjm.ling.ESpan

import cats.implicits._

package object debate extends PackagePlatformExtensions {

  @Lenses case class Lobby(
    trackedDebaters: Set[String],
    scheduledRooms: Vector[RoomMetadata],
    openRooms: Vector[RoomMetadata]
  )
  object Lobby {
    def init = Lobby(Set(), Vector(), Vector())
  }

  sealed trait MainChannelRequest
  case class RegisterDebater(debaterName: String) extends MainChannelRequest

  @JsonCodec sealed trait RoomStatus {
    import RoomStatus._
    override def toString = this match {
      case SettingUp => "setting up"
      case InProgress => "in progress"
      case Complete => "complete"
    }

    def isComplete = this match {
      case Complete => true
      case _ => false
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
    status: RoomStatus,
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

  def simpleTokenize(x: String): Vector[String] = {
    val res = x
      .split("\n")
      .toVector
      .map(Vector(_))
      .intercalate(Vector("\n"))
      .filter(_.nonEmpty)
      .flatMap(
        _.split(" +").toVector
      )
    res
  }

  def bigTokenize(x: String): Vector[String] = {
    val res = x
      .split("\n")
      .toVector
      .map(Vector(_))
      .intercalate(Vector("\n"))
      .filter(_.nonEmpty)
      .flatMap(
        _.split("""(?<=[\\.!?;]) +""").toVector
      )
    res
  }

  def biggTokenize(x: String): Vector[String] = {
    val res = x
      .split("\n")
      .toVector
      .map(Vector(_))
      .intercalate(Vector("\n"))
      .filter(_.nonEmpty)
    res
  }

  def bigggTokenize(x: String): Vector[String] = {
    val res = x
      .split("\n\n")
      .toVector
      .map(Vector(_))
      .intercalate(Vector("\n\n"))
      .filter(_.nonEmpty)
    res
  }
}
