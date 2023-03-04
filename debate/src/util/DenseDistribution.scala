package debate
package util

import cats.data.NonEmptyVector
import scala.util.Random

class DenseDistribution[A] private (probs: NonEmptyVector[(A, Double)]) {
  def probAt(i: Int): Option[Double] = probs.get(i).map(_._2)
  def prob(a: A): Double             = probs.find(_ == a).map(_._2).getOrElse(0.0)
  def sample(rng: Random): A = {
    val randomDouble = rng.nextDouble()
    var sum          = 0.0
    probs
      .toVector
      .foreach { case (a, prob) =>
        sum += prob
        if (randomDouble < sum) {
          return a
        }
      }
    return probs.last._1
  }
  // def prob(a: A): Double = probs.getOrElse(a, 0.0)
  // def support            = probs.keySet
}
object DenseDistribution {
  def apply[A](itemsWithProbs: NonEmptyVector[(A, Double)]) = new DenseDistribution(itemsWithProbs)
  def fromSoftmax[A](items: NonEmptyVector[A], getScore: A => Double) = {
    val scores    = items.map(getScore)
    val expScores = scores.map(math.exp)
    val sumOfExps = expScores.reduce
    val probs = expScores.map { expScore =>
      expScore / sumOfExps
    }
    new DenseDistribution(items.zipWith(probs)(_ -> _))
  }
}
