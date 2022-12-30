package debate.util

import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import debate.Helpers.ClassSetInterpolator
import japgolly.scalajs.react.Callback

object Checkbox2 {

  val S = debate.Styles
  def apply(
    toggle: StateSnapshot[Boolean],
    labelOpt: Option[String] = None,
    didUpdate: Boolean => Callback = _ => Callback.empty
  ) = mod()(toggle, labelOpt, didUpdate)

  def mod(
    div: TagMod = c"form-check",
    box: TagMod = c"form-check-input",
    label: TagMod = c"form-check-label"
  )(
    toggle: StateSnapshot[Boolean],
    labelOpt: Option[String] = None,
    didUpdate: Boolean => Callback = _ => Callback.empty
  ) =
    <.div(div)(
      <.input(box)(
        ^.`type` := "checkbox",
        labelOpt.whenDefined(^.value := _),
        ^.checked := toggle.value,
        ^.onChange --> (toggle.modState(!_) >> didUpdate(!toggle.value))
      ),
      labelOpt.fold(TagMod.empty)(l => <.label(label)(l))
    )
}
