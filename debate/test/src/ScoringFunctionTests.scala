package debate

import munit.CatsEffectSuite

class ScoringFunctionTests extends CatsEffectSuite {

  val scoringFunction = ScoringFunction.LogScoreWithLinearPenalty(
    baseCoefficient = 1.0,
    constant = 0.0,
    logBase = 2.0,
    perTurnPenalty = 0.25
  )
  val turnNum = 1
  val probs   = Vector(0.4, 0.6)

  test("Deltas to make next turn worthwhile indeed make it worthwhile") {
    // TODO expand with property testing (scalacheck)
    val currentEV = scoringFunction.expectedValue(probs, turnNum)
    ScoringFunction
      .deltasForNextTurnToBeWorthwhile(scoringFunction, probs, turnNum)
      .zipWithIndex
      .collect { case (Some(delta), index) =>
        delta -> index
      }
      .foreach { case (delta, index) =>
        val nextProbs = Utils.adjustProbability(probs, index, probs(index) + delta)
        val nextEV    = scoringFunction.expectedValue(nextProbs, turnNum + 1)
        assert {
          clue(delta);
          clue(index);
          clue(probs);
          clue(nextProbs);
          clue(nextEV) >= clue(currentEV)
        }
      }
  }
  // TODO also test that it is minimal
  // TODO also test that it is found if it exists

}
