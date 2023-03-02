package debate
package util

import jjm.implicits._

class SparseDistribution[A] private (val probs: Map[A, Double]) {
  def prob(a: A): Double = probs.getOrElse(a, 0.0)
  def support            = probs.keySet
}
object SparseDistribution {
  def apply[A](probs: Map[A, Double]) = {
    val sum = probs.values.sum
    new SparseDistribution(probs.mapVals(_ / sum))
  }
}
