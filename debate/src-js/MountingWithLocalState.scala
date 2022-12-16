package debate

/** Inspired by [jjm.ui]'s [LocalState.scala] and [Mounting.scala]. Combines
  * them.
  */

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot

class MountingWithLocalState[A] {
  type State = A
  type Context = A => Callback
  case class Props(
      initialValue: A,
      render: StateSnapshot[A] => VdomElement,
      onMount: AsyncCallback[A],
      shouldRefresh: A => Boolean
  )

  val Component = ScalaComponent
    .builder[Props]("MountingWithLocalState")
    .initialStateFromProps(_.initialValue)
    .render { $ => $.props.render(StateSnapshot.of($)) }
    .componentDidMount($ => {
      val ace =
        for {
          a <- $.props.onMount
          _ <- $.setState(a).async
        } yield ()
      ace.toCallback
    })
    // TODO what type is the dollar sign? can we just call an internal [render]?
    .componentDidUpdate { $ =>
      if (
        $.prevProps.initialValue != $.currentProps.initialValue &&
        $.currentProps.shouldRefresh($.currentState)
      ) {
        $.setState($.currentProps.initialValue)
      } else Callback.empty
    }
    .build

  def make(
      initialValue: A,
      render: StateSnapshot[A] => VdomElement,
      onMount: AsyncCallback[A],
      shouldRefresh: A => Boolean
  ) =
    Component(Props(initialValue, render, onMount, shouldRefresh))
}
