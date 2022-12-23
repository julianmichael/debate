package debate
package util

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import io.circe.Encoder
import io.circe.Decoder

class LocalState2[A] {
  type State = A
  type Context = A => Callback
  case class Props(
    initialValue: A,
    didUpdate: (A, A) => Callback = (_: A, _: A) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true,
    render: StateSnapshot[A] => VdomElement
  )

  val Component = ScalaComponent
    .builder[Props]("Local State")
    .initialStateFromProps(_.initialValue)
    .render { $ => $.props.render(StateSnapshot.of($)) }
    .componentDidUpdate { $ =>
      val refresh = if ($.prevProps.initialValue != $.currentProps.initialValue &&
            $.currentProps.shouldRefresh($.currentState)) {
        $.setState($.currentProps.initialValue)
      } else Callback.empty
      $.currentProps.didUpdate($.prevState, $.currentState) >> refresh
    }
    .build

  def make(
    initialValue: A,
    didUpdate: (A,A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true)(
    render: StateSnapshot[A] => VdomElement
  ) = {
    Component(Props(initialValue, didUpdate, shouldRefresh, render))
  }

  // TODO sync with local storage, session storage, etc. instead of cookies

  import org.scalajs.dom

  def syncedWithStorage(
    storage: dom.raw.Storage,
    key: String,
    defaultValue: A,
    didUpdate: (A,A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true)(
    render: StateSnapshot[A] => VdomElement)(
    implicit enc: Encoder[A], dec: Decoder[A]
  ) = {
    import io.circe.syntax._
    import io.circe.parser
    val initialValue =
        Option(storage.getItem(key))
            .flatMap(str => parser.decode[A](str) match {
                case Right(result) => Some(result)
                case Left(err) => {
                    System.err.println(
                        s"Failed to decode LocalState from storage at key $key. " +
                        "Printing error stack trace."
                    )
                    err.printStackTrace()
                    None
                }
            })
            .getOrElse(defaultValue)
    val storeValue = (a: A) => Callback(storage.setItem(key, a.asJson.noSpaces))
    val didUpdateFull = (p: A, c: A) => didUpdate(p, c) >> storeValue(c)
    Component(Props(initialValue, didUpdateFull, shouldRefresh, render))
  }

  def syncedWithLocalStorage(
    key: String,
    defaultValue: A,
    didUpdate: (A,A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true)(
    render: StateSnapshot[A] => VdomElement)(
    implicit enc: Encoder[A], dec: Decoder[A]
  ) = syncedWithStorage(
        dom.window.localStorage, key, defaultValue,
        didUpdate, shouldRefresh)(
        render
  )

  def syncedWithSessionStorage(
    key: String,
    defaultValue: A,
    didUpdate: (A,A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true)(
    render: StateSnapshot[A] => VdomElement)(
    implicit enc: Encoder[A], dec: Decoder[A]
  ) = syncedWithStorage(
        dom.window.sessionStorage, key, defaultValue,
        didUpdate, shouldRefresh)(
        render
  )
}
