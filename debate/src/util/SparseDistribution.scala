package debate.util

import cats.implicits._

import cats.data.NonEmptyMap
import cats.Order
import scala.collection.immutable.SortedMap
import io.circe.Encoder
import io.circe.Decoder

class SparseDistribution[A] private (val probs: NonEmptyMap[A, Double]) {
  def prob(a: A): Double = probs(a).getOrElse(0.0)
  // def support            = probs.keySet
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
