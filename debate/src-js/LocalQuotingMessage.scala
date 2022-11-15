package debate

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import jjm.ling.ESpan

import cats.implicits._

/** Local state HOC for the speech a debater is currently constructing. This
  * exists to keep in sync the overlying `spans` state (of currently highlighted
  * spans in the story) and the underlying message state (of the text being
  * written by the debater), without requiring the full message state to be
  * above the `spans` state in the VDOM tree. My thinking was that that should
  * help prevent the story VDOM from being recomputed with each character being
  * typed. But... not sure if that actually was an issue; was just throwing
  * things at the wall. This can probably be removed at some point.
  */
object LocalQuotingMessage {
  type State = String
  type Context = String => Callback
  case class Props(
      spans: StateSnapshot[Set[ESpan]],
      initialValue: String,
      shouldRefresh: String => Boolean,
      didUpdate: String => Callback,
      render: StateSnapshot[String] => VdomElement
  )

  val Component = ScalaComponent
    .builder[Props]("Local Quoting Message")
    .initialStateFromProps(_.initialValue)
    .render { $ => $.props.render(StateSnapshot.of($)) }
    .componentWillReceiveProps { $ =>
      if (
        $.currentProps.initialValue != $.nextProps.initialValue &&
        $.nextProps.shouldRefresh($.state)
      ) {
        $.setState($.nextProps.initialValue)
      } else {
        val messageSpans: Set[ESpan] = SpeechSegment
          .getSegmentsFromString($.state)
          .collect { case SpeechSegment.Quote(span) => span }
          .toSet
        val newSpans = $.nextProps.spans.value -- messageSpans
        if (newSpans.nonEmpty) {
          $.modState(_ + newSpans.toVector.sorted.foldMap(span2text))
        } else Callback.empty
      }
    }
    .componentDidUpdate { $ =>
      val messageSpans = SpeechSegment
        .getSegmentsFromString($.currentState)
        .collect { case SpeechSegment.Quote(span) => span }
        .toSet

      val spanCb = if (messageSpans != $.currentProps.spans.value) {
        $.currentProps.spans.setState(messageSpans)
      } else Callback.empty

      spanCb >> $.currentProps.didUpdate($.currentState)
    }
    .build

  def make(
      spans: StateSnapshot[Set[ESpan]],
      initialValue: String,
      shouldRefresh: String => Boolean = (_: String) => true,
      didUpdate: String => Callback
  )(
      render: StateSnapshot[String] => VdomElement
  ) = {
    Component(Props(spans, initialValue, shouldRefresh, didUpdate, render))
  }
}
