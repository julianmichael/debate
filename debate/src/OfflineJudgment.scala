package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses
import io.circe.Encoder
import io.circe.Decoder

sealed trait OfflineJudgingMode {
  override def toString =
    this match {
      case OfflineJudgingMode.Timed =>
        "timed"
      case OfflineJudgingMode.Stepped =>
        "stepped"
    }
}
object OfflineJudgingMode {
  case object Timed   extends OfflineJudgingMode
  case object Stepped extends OfflineJudgingMode

  def all = List(Timed, Stepped)

  def fromString(x: String) = all.find(_.toString == x)

  implicit val offlineJudgingModeEncoder: Encoder[OfflineJudgingMode] = Encoder
    .encodeString
    .contramap[OfflineJudgingMode](_.toString)
  implicit val offlineJudgingModeDecoder: Decoder[OfflineJudgingMode] = Decoder
    .decodeString
    .emap(x => fromString(x).toRight(s"Invalid offline judging mode: $x"))

}

@Lenses
@JsonCodec
case class OfflineJudgment(
  mode: OfflineJudgingMode,
  startTimeMillis: Long,
  numContinues: Int,
  result: Option[OfflineJudgingResult]
)
object OfflineJudgment

@JsonCodec
case class OfflineJudgingResult(distribution: Vector[Double], explanation: String, timestamp: Long)
