package debate

object Utils {
  def normalize(probs: Vector[Double]) = {
    val sum = probs.sum
    probs.map(_ / sum)
  }

  def adjustProbability(
    dist: Vector[Double],
    index: Int,
    newProb: Double
  ): Vector[Double] = {
    val prob = dist(index)
    normalize(
      dist
        .map(x =>
          if (prob == 1.0) (1.0 - newProb) / ((dist.size - 1))
          else x * (1 - newProb) / (1 - prob)
        )
        .updated(index, newProb)
    )
  }
}
