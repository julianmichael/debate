package debate

import cats.implicits._
import monocle.macros.Lenses
import io.circe.generic.JsonCodec
import monocle.macros.GenPrism

/** Scoring function for the judge (currently, logically applies at the end of
  * the debate). These are based on proper scoring rules to elicit calibrated
  * probabilities.
  * https://en.wikipedia.org/wiki/Scoring_rule#Proper_scoring_rules
  *
  * We also apply a penalty that is roughly linear in the length of the debate,
  * to incentivize efficient information-gathering and encourage the judge to
  * end the debate when they believe it is no longer useful to continue.
  */
@JsonCodec sealed trait ScoringFunction {
  def max: Double
  def eval(turnNumber: Int, distribution: Vector[Double], outcome: Int): Double
  def perTurnPenalty: Double

  def expectedValue(distribution: Vector[Double], turnNumber: Int): Double = {
    distribution.zipWithIndex.foldMap { case (p, i) => p * eval(turnNumber, distribution, i) }
  }
}
object ScoringFunction {
  @Lenses @JsonCodec case class SphericalScoreWithLinearPenalty(
      baseCoefficient: Double,
      perTurnPenalty: Double
  ) extends ScoringFunction {
    def max: Double = baseCoefficient
    def eval(
        turnNumber: Int,
        distribution: Vector[Double],
        outcome: Int
    ): Double = {
      val norm = math.sqrt(distribution.foldMap(math.pow(_, 2)))
      baseCoefficient * distribution(
        outcome
      ) / norm - (perTurnPenalty * turnNumber)
    }
  }

  @Lenses @JsonCodec case class QuadraticScoreWithLinearPenalty(
      baseCoefficient: Double,
      perTurnPenalty: Double
  ) extends ScoringFunction {
    def max: Double = baseCoefficient
    def eval(
        turnNumber: Int,
        distribution: Vector[Double],
        outcome: Int
    ): Double = {
      val sqNorm = distribution.foldMap(math.pow(_, 2))
      baseCoefficient * ((2 * distribution(
        outcome
      )) - sqNorm) - (perTurnPenalty * turnNumber)
    }
  }

  @Lenses @JsonCodec case class LogScoreWithLinearPenalty(
      baseCoefficient: Double,
      constant: Double, // in order to make it a bit nicer since log score is always negative.
      logBase: Double,
      perTurnPenalty: Double
  ) extends ScoringFunction {
    def max: Double = constant
    def eval(
        turnNumber: Int,
        distribution: Vector[Double],
        outcome: Int
    ): Double = {
      baseCoefficient * (math.log(distribution(outcome)) / math.log(
        logBase
      )) + constant - (perTurnPenalty * turnNumber)
    }
  }

  val sphericalScoreWithLinearPenalty =
    GenPrism[ScoringFunction, SphericalScoreWithLinearPenalty]
  val quadraticScoreWithLinearPenalty =
    GenPrism[ScoringFunction, QuadraticScoreWithLinearPenalty]
  val logScoreWithLinearPenalty =
    GenPrism[ScoringFunction, LogScoreWithLinearPenalty]


  /**
    * Return a vector of the absolute increase in probability of each option that would be required
    * to justify (in expectation) the cost of taking another turn.
    *
    * @param scoringFunction
    * @param probs
    * @param turnNum
    * @return vector of increase in probability for each answer, aligned with probs; None if impossible
    */
  def deltasForNextTurnToBeWorthwhile(
    scoringFunction: ScoringFunction,
    probs: Vector[Double],
    turnNum: Int
  ): Vector[Option[Double]] = {
    val currentEV = scoringFunction.expectedValue(probs, turnNum)
    val hypotheticalScores = probs.indices.map(index =>
      scoringFunction
        .eval(turnNum + 1, probs, index)
    )

    // just do it by guess and check starting low
    val reqdProbDeltas =
      probs.indices.map { answerIndex =>
        val scoresNextTurnAfterDeltaIncrease = LazyList
          .from(0 to 100)
          .map(_.toDouble / 100.0)
          .map { delta =>
            delta -> scoringFunction.expectedValue(
              Utils.adjustProbability(
                probs,
                answerIndex,
                probs(answerIndex) + delta
              ),
              turnNum + 1
            )
          }
        scoresNextTurnAfterDeltaIncrease
          .find(_._2 >= currentEV)
          .map(_._1)
      }.toVector

    reqdProbDeltas
  }
}
