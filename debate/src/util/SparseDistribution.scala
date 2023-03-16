package debate.util

import scala.collection.immutable.SortedMap

import cats.Order
import cats.data.NonEmptyMap
import cats.data.NonEmptySet
import cats.implicits._

import io.circe.Decoder
import io.circe.Encoder

class SparseDistribution[A] private (val probs: NonEmptyMap[A, Double]) {
  def prob(a: A): Double = probs(a).getOrElse(0.0)
  // def support            = probs.keySet

  def withProbability(item: A, newProb: Double): SparseDistribution[A] = {
    val oldProb = prob(item)
    // val newNormalizer = 1.0 + newProb - oldProb
    new SparseDistribution(
      probs
        .map(x =>
          if (oldProb == 1.0)
            (1.0 - newProb) / (probs.size - 1)
          else
            x * (1 - newProb) / (1 - oldProb)
        )
        .add(item -> newProb)
    )
    // normalize(
    //   dist
    //     .map(x =>
    //       if (prob == 1.0)
    //         (1.0 - newProb) / (dist.size - 1)
    //       else
    //         x * (1 - newProb) / (1 - prob)
    //     )
    //     .updated(index, newProb)
    // )
  }
}
object SparseDistribution {
  def apply[A](values: NonEmptyMap[A, Double]): SparseDistribution[A] = {
    val sum = values.unorderedFold
    new SparseDistribution(values.map(_ / sum))
  }

  def fromMap[A: Order](probs: Map[A, Double]): Option[SparseDistribution[A]] = {
    import cats.Order.catsKernelOrderingForOrder
    val sortedMap = SortedMap.from(probs)
    NonEmptyMap.fromMap(sortedMap).map(apply)
  }

  def uniform[A: Order](support: NonEmptySet[A]) = apply(
    support.reduceMap(a => NonEmptyMap.of(a -> 1.0))
  )

  implicit def sparseDistributionEncoder[A: Encoder]: Encoder[SparseDistribution[A]] =
    implicitly[Encoder[List[(A, Double)]]]
      .contramap[SparseDistribution[A]](_.probs.toSortedMap.toList)

  implicit def sparseDistributionDecoder[A: Decoder: Order]: Decoder[SparseDistribution[A]] =
    implicitly[Decoder[List[(A, Double)]]].emap(items =>
      NonEmptyMap
        .fromMap(SortedMap.from(items))
        .toRight("Sparse distribution has no support")
        .map(SparseDistribution(_))
    )
}
