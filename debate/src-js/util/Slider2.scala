package debate.util

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactEventFromInput
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import japgolly.scalajs.react.feature.ReactFragment

// import debate.Utils.ClassSetInterpolator

object Slider2 {
  val S = debate.Styles
  val V = new jjm.ui.View(S)
  def mod(
    slider: TagMod = TagMod.empty,
    textSpan: TagMod = TagMod.empty,
    textInput: TagMod = S.shortTextField,
    textInputFromValue: Option[Double] => TagMod =
      V.LiveTextField.defaultInputStyleFromValue[Double](_),
    label: TagMod = TagMod.empty
  )(
    value: StateSnapshot[Double],
    min: Double,
    max: Double,
    labelOpt: Option[String] = None,
    numSigFigs: Int = 3,
    didUpdate: Double => Callback = _ => Callback.empty
  ) = {
    val magnitude      = scala.math.pow(10, numSigFigs).toInt
    def int(x: Double) = (x * magnitude).toInt
    def double(n: Int) = n.toDouble / magnitude
    ReactFragment(
      V.LiveTextField
        .Double
        .mod(
          span = textSpan,
          input = textInput,
          inputFromValue = textInputFromValue,
          label = label
        )(value, labelOpt),
      <.input(slider)(
        ^.`type` := "range",
        ^.min    := int(min),
        ^.max    := int(max),
        ^.value  := int(value.value),
        ^.onChange ==> { (e: ReactEventFromInput) =>
          val newVal = double(e.target.value.toInt)
          value.setState(newVal) >> didUpdate(newVal)
        }
      )
    )
  }

  def apply(
    value: StateSnapshot[Double],
    min: Double,
    max: Double,
    labelOpt: Option[String] = None,
    numSigFigs: Int = 3,
    didUpdate: Double => Callback = _ => Callback.empty
  ) = mod()(value, min, max, labelOpt, numSigFigs, didUpdate)
}
