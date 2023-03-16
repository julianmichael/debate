package debate.util

import scala.util.Random

import cats.data.NonEmptyVector
import cats.implicits._

import jjm.implicits._

class DenseDistribution[A] private (probs: NonEmptyVector[(A, Double)]) {
  def withTemperature(t: Double): DenseDistribution[A] = DenseDistribution(
    probs.map { case (a, p) =>
      a -> math.pow(p, 1.0 / t)
    }
  )

  def probAt(i: Int): Option[Double] = probs.get(i).map(_._2)
  def prob(a: A): Double             = probs.find(_ == a).map(_._2).getOrElse(0.0)

  def sampleWithIndex(rng: Random): (A, Int) = {
    val randomDouble = rng.nextDouble()
    var sum          = 0.0
    probs
      .toVector
      .zipWithIndex
      .foreach { case ((a, prob), i) =>
        sum += prob
        if (randomDouble < sum) {
          return a -> i
        }
      }
    return probs.last._1 -> (probs.size.toInt - 1)
  }

  def sample(rng: Random): A = sampleWithIndex(rng)._1

  def sampleWithoutReplacement(n: Int, rng: Random): List[A] = {
    var remaining = probs.toVector
    var sampled   = List.empty[A]
    var nToGo     = n
    while (nToGo > 0 && remaining.nonEmpty) {
      nToGo = nToGo - 1
      // not deconstructing in assignment because of some compiler foible?
      val (next, idx) = DenseDistribution(NonEmptyVector.fromVector(remaining).get)
        .sampleWithIndex(rng)
      sampled = next :: sampled
      remaining = remaining.remove(idx)
    }
    sampled
  }
  // def prob(a: A): Double = probs.getOrElse(a, 0.0)
  // def support            = probs.view.map(_._1).toSet
}
object DenseDistribution {
  def apply[A](itemsWithProbs: NonEmptyVector[(A, Double)]) = {
    val sum = itemsWithProbs.foldMap(_._2)
    new DenseDistribution(
      itemsWithProbs.map { case (a, p) =>
        a -> (p / sum)
      }
    )
  }
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
