package debate

import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.JsonCodec
import monocle.macros.Lenses

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
case class OfflineJudgment(
  mode: OfflineJudgingMode,
  startTimeMillis: Long,
  judgments: Vector[JudgeFeedback]
) {
  def result: Option[JudgeFeedback] = judgments.lastOption.filter(_.endDebate)
}
object OfflineJudgment {
  implicit val offlineJudgmentEncoder: Encoder[OfflineJudgment] = {
    import io.circe.generic.semiauto._
    deriveEncoder[OfflineJudgment]
  }
  implicit val offlineJudgmentDecoder: Decoder[OfflineJudgment] = {
    import io.circe.generic.semiauto._
    deriveDecoder[OfflineJudgment].or(
      Decoder.forProduct3("mode", "startTimeMillis", "result")(
        (mode: OfflineJudgingMode, startTimeMillis: Long, result: Option[OfflineJudgingResult]) =>
          OfflineJudgment(
            mode,
            startTimeMillis,
            result
              .map(res =>
                JudgeFeedback(
                  res.distribution,
                  DebateSpeech("", res.timestamp, Vector(SpeechSegment.Text(res.explanation))),
                  true
                )
              )
              .toVector
          )
      )
    )
  }
}

@JsonCodec
case class OfflineJudgingResult(distribution: Vector[Double], explanation: String, timestamp: Long)
