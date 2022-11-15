package debate

import jjm.ling.ISpan

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._

import monocle.Lens
import monocle.Prism

/** HOC for span selection modified from the original jjm.ui.SpanSelection just
  * to add spans to a span state controlled by the caller.
  */
object SpanSelection2 {

  sealed trait Status
  case object NoSpan extends Status
  case class Selecting(anchor: Int, endpoint: Int) extends Status
  object Selecting {
    // For some reason, macros don't work, and I have to define these members
    // in order for the lenses to actually compile. something is messed up
    // private[this] val indexSet: (Index => Selecting => Selecting) =
    //   (i: Index) => (s: Selecting) => s.copy(index = i)
    (a: Int) => (s: Selecting) => s.copy(anchor = a)
    (e: Int) => (s: Selecting) => s.copy(endpoint = e)
    // val index = Lens[Selecting, Index](_.index)(i => s => s.copy(index = i))
    val anchor = Lens[Selecting, Int](_.anchor)(a => s => s.copy(anchor = a))
    val endpoint =
      Lens[Selecting, Int](_.endpoint)(e => s => s.copy(endpoint = e))
  }
  object Status {
    val noSpan = Prism[Status, NoSpan.type](s =>
      s match { case NoSpan => Some(NoSpan); case _ => None }
    )(identity)
    val selecting = Prism[Status, Selecting](s =>
      s match { case s @ Selecting(_, _) => Some(s); case _ => None }
    )(identity)
  }

  // case class State(
  //   // spans: Map[Index, List[ISpan]],
  //   status: Status
  // )

  case class Context(
      // setSpan: Map[Index, List[ISpan]] => Callback,
      hover: Int => Callback,
      touch: Int => Callback,
      cancel: Callback
  )

  case class Props(
      // spans: Map[Index, List[ISpan]],
      isEnabled: Boolean,
      // enableSpanOverlap: Boolean = true,
      addSpan: ISpan => Callback,
      // update: State => Callback,
      render: (Status, Context) => VdomElement
  )

  class Backend(scope: BackendScope[Props, Status]) {

    def hover(props: Props, state: Status)(endpoint: Int) =
      scope.modState {
        case Selecting(anchor, _) => Selecting(anchor, endpoint)
        case x                    => x
      }

    def touch(props: Props, state: Status)(wordIndex: Int): Callback =
      state match {
        case NoSpan =>
          scope.setState(Selecting(wordIndex, wordIndex)) // start highlighting
        case Selecting(x, y) =>
          props.addSpan(ISpan(x, y)) >> scope.setState(NoSpan) // finish span
        case _ =>
          Callback.empty
      }

    def cancel = scope.setState(NoSpan)

    def render(props: Props, state: Status) =
      props.render(
        state,
        Context(
          hover(props, state),
          touch(props, state),
          cancel
        )
      )
  }

  val Component = ScalaComponent
    .builder[Props]("Span Selection 2")
    .initialState(NoSpan: Status)
    .renderBackend[Backend]
    .build

  def make(isEnabled: Boolean, addSpan: ISpan => Callback)(
      render: (Status, Context) => VdomElement
  ) = {
    Component(Props(isEnabled, addSpan, render))
  }
}
