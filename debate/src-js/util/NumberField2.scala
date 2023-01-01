package debate.util

import japgolly.scalajs.react.ReactEventFromInput
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import debate.Utils.ClassSetInterpolator
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.feature.ReactFragment

object NumberField2 {
  val S = debate.Styles
  def mod(label: TagMod = c"col-form-label", input: TagMod = c"col form-control")(
    value: StateSnapshot[Int],
    labelOpt: Option[String] = None,
    didUpdate: Int => Callback = _ => Callback.empty
  ) = ReactFragment(
    labelOpt.map(l => <.label(label)(l)),
    <.input(input, c"ml-2".when(labelOpt.nonEmpty))(
      ^.`type` := "number",
      ^.min    := 1,
      ^.value  := value.value,
      ^.onChange ==> { (e: ReactEventFromInput) =>
        val newVal = e.target.value.toInt
        value.setState(newVal) >> didUpdate(newVal)
      }
    )
  )
  def apply(
    value: StateSnapshot[Int],
    labelOpt: Option[String] = None,
    didUpdate: Int => Callback = _ => Callback.empty
  ) = mod()(value, labelOpt, didUpdate)
}
