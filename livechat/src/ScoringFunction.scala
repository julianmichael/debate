package livechat

import cats.implicits._
import monocle.macros.Lenses
import io.circe.generic.JsonCodec
import monocle.macros.GenPrism

// scoring function for the judge at a turn in the debate
// constraint: must be nonnegative and bounded above (at `max`)

@JsonCodec sealed trait ScoringFunction {
  def max: Double
  def eval(turnNumber: Int, distribution: Vector[Double], outcome: Int): Double
  def perTurnPenalty: Double
}
object ScoringFunction {
  @Lenses @JsonCodec case class SphericalScoreWithLinearPenalty(
    baseCoefficient: Double,
    perTurnPenalty: Double
  ) extends ScoringFunction {
    def max: Double = baseCoefficient
    def eval(turnNumber: Int, distribution: Vector[Double], outcome: Int): Double = {
      val norm = math.sqrt(distribution.foldMap(math.pow(_, 2)))
      baseCoefficient * distribution(outcome) / norm - (perTurnPenalty * turnNumber)
    }
  }

  @Lenses @JsonCodec case class QuadraticScoreWithLinearPenalty(
      baseCoefficient: Double,
      perTurnPenalty: Double
    ) extends ScoringFunction {
      def max: Double = baseCoefficient
      def eval(turnNumber: Int, distribution: Vector[Double], outcome: Int): Double = {
        val sqNorm = distribution.foldMap(math.pow(_, 2))
        baseCoefficient * ((2 * distribution(outcome)) - sqNorm) - (perTurnPenalty * turnNumber)
      }
    }

  @Lenses @JsonCodec case class LogScoreWithLinearPenalty(
    baseCoefficient: Double,
    constant: Double,
    logBase: Double,
    perTurnPenalty: Double
  ) extends ScoringFunction {
    def max: Double = constant
    def eval(turnNumber: Int, distribution: Vector[Double], outcome: Int): Double = {
      baseCoefficient * (math.log(distribution(outcome)) / math.log(logBase)) + constant - (perTurnPenalty * turnNumber)
    }
  }

  val sphericalScoreWithLinearPenalty = GenPrism[ScoringFunction, SphericalScoreWithLinearPenalty]
  val quadraticScoreWithLinearPenalty = GenPrism[ScoringFunction, QuadraticScoreWithLinearPenalty]
  val logScoreWithLinearPenalty = GenPrism[ScoringFunction, LogScoreWithLinearPenalty]
}
