// package livechat

// import japgolly.scalajs.react.vdom.html_<^._
// import japgolly.scalajs.react._
// import japgolly.scalajs.react.extra.StateSnapshot

// class LocalState2[A] {
//   type State = A
//   type Context = A => Callback
//   case class Props(
//     initialValue: A,
//     shouldRefresh: A => Boolean,
//     didUpdate: A => Callback,
//     render: StateSnapshot[A] => VdomElement
//   )

//   val Component = ScalaComponent
//     .builder[Props]("Local State")
//     .initialStateFromProps(_.initialValue)
//     .render { $ => $.props.render(StateSnapshot.of($)) }
//     .componentWillReceiveProps { $ =>
//       if ($.currentProps.initialValue != $.nextProps.initialValue &&
//             $.nextProps.shouldRefresh($.state)) {
//         $.setState($.nextProps.initialValue)
//       } else Callback.empty
//     }
//     .componentDidUpdate($ => $.currentProps.didUpdate($.currentState))
//     .build

//   def make(
//     initialValue: A,
//     shouldRefresh: A => Boolean = (_: A) => true,
//     didUpdate: A => Callback)(
//     render: StateSnapshot[A] => VdomElement
//   ) = {
//     Component(Props(initialValue, shouldRefresh, didUpdate, render))
//   }
// }
