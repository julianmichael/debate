package debate.util

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import debate.util.SparseDistribution

/** HOC for probability sliders, where each outcome gets a slider and they're
  * constrained to constitute a probability distribution.
  * Uses SparseDistribution.
  */
object ProbabilitySliders2 {

  val S = debate.Styles
  val V = new jjm.ui.View(S)

  // TODO have checkboxes for locking probabilities so they don't change when you change others
  // This would require making this stateful.
  case class Context[A](
    item: A,
    index: Int,
    prob: Double,
    setProb: Double => Callback
    // toggleLock: Callback
  )

  def mod[A](
    div: TagMod = S.probSlidersDiv
  )(probs: StateSnapshot[SparseDistribution[A]])(render: Context[A] => VdomElement) =
    <.div(div)(
      probs
        .value
        .probs
        .toSortedMap
        .toList
        .zipWithIndex
        .toVdomArray { case ((item, prob), index) =>
          // probs.zoomStateO(Optics.index(index)).get
          render(Context(item, index, prob, p => probs.modState(_.withProbability(item, p))))
        }
    )

  def apply[A](probs: StateSnapshot[SparseDistribution[A]])(render: Context[A] => VdomElement) =
    mod()(probs)(render)
}
