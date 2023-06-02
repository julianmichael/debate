package debate
package view.debate

// import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

import debate.util.ListConfig

// import jjm.ling.ESpan
// import jjm.ling.ISpan
// import jjm.ling.Span
// import jjm.ui.Rgba

// import debate.util._

object ScratchpadPanel {

  import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def apply(
    source: Vector[String],
    currentMessage: StateSnapshot[String],
    scratchpad: StateSnapshot[Vector[Vector[SpeechSegment]]]
  ) =
    ListConfig[Vector[SpeechSegment]].nice(scratchpad, Vector(), 0, includeAddButton = false) {
      case ListConfig.Context(item, _) =>
        <.div(c"card-body")(
          <.p(c"card-text")(DebateRoundView.makeSpeechContentHtml(source, item.value)),
          <.button(c"btn btn-secondary")(
            "Load ",
            <.i(c"bi bi-arrow-right"),
            ^.onClick --> currentMessage.modState(_ + SpeechSegments.getString(item.value))
          )
        )
    }
}
