package debate

import cats.Foldable
import cats.Functor
import cats.implicits._

import jjm.ling.Span
import jjm.ling.Text

object Utils extends UtilsPlatformExtensions {

  // more efficient than the jjm default, which I didn't even realize was a problem haha
  def renderSpan(tokens: Vector[String], span: Span) = Text
    .render(tokens.slice(span.begin, span.endExclusive))

  def normalize(probs: Vector[Double]) = {
    val sum = probs.sum
    probs.map(_ / sum)
  }

  def adjustProbability(dist: Vector[Double], index: Int, newProb: Double): Vector[Double] = {
    val prob = dist(index)
    normalize(
      dist
        .map(x =>
          if (prob == 1.0)
            (1.0 - newProb) / (dist.size - 1)
          else
            x * (1 - newProb) / (1 - prob)
        )
        .updated(index, newProb)
    )
  }

  def zScores[F[_]: Foldable: Functor](x: F[Double]): F[Double] = {
    val mean = x.fold / x.size
    val stdDev = math.sqrt(
      x.foldMap { xi =>
        math.pow(xi - mean, 2)
      } / x.size
    )
    return x.map { xi =>
      (xi - mean) / stdDev
    }
  }
}
