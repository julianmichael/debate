package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

@Lenses
@JsonCodec
case class OfflineJudgment(
  startTimeMillis: Long,
  numContinues: Int,
  result: Option[OfflineJudgingResult]
)
object OfflineJudgment

@JsonCodec
case class OfflineJudgingResult(distribution: Vector[Double], explanation: String, timestamp: Long)
