package debate

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.ScalaCssReact._


import jjm.ui.LocalState

/** HOC for probability sliders, where each outcome gets a slider and they're
  * constrained to constitute a probability distribution
  */
object ProbabilitySliders {

  val S = debate.Styles
  val V = new jjm.ui.View(S)
  val LocalString = new LocalState[String]

  // TODO have checkboxes for locking probabilities so they don't change when you change others
  // This would require making this stateful.
  case class Context(
      index: Int,
      prob: Double,
      setProb: Double => Callback
      // toggleLock: Callback
  )

  def mod(div: TagMod = S.probSlidersDiv)(probs: StateSnapshot[Vector[Double]])(
    render: Context => VdomElement
  ) = {
    <.div(div)(
      probs.value.zipWithIndex.toVdomArray { case (prob, index) =>
        // probs.zoomStateO(Optics.index(index)).get
        render(
          Context(
            index,
            prob,
            p => probs.setState(Utils.adjustProbability(probs.value, index, p))
          )
        )
      }
    )
  }

  def apply(probs: StateSnapshot[Vector[Double]])(
      render: Context => VdomElement
  ) = mod()(probs)(render)
}
