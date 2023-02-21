package debate

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
}
