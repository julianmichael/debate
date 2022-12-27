package debate
package util

import io.circe.Decoder
import io.circe.Encoder
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._

class LocalState2[A] {
  type State   = A
  type Context = A => Callback
  case class Props(
    initialValue: A,
    didUpdate: (A, A) => Callback = (_: A, _: A) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true,
    render: StateSnapshot[A] => VdomElement
  )

  val Component =
    ScalaComponent
      .builder[Props]("Local State 2")
      .initialStateFromProps(_.initialValue)
      .render { $ =>
        $.props.render(StateSnapshot.of($))
      }
      .componentDidUpdate { $ =>
        val refresh =
          if (
            $.prevProps.initialValue != $.currentProps.initialValue &&
            $.currentProps.shouldRefresh($.currentState)
          ) {
            $.setState($.currentProps.initialValue)
          } else
            Callback.empty
        $.currentProps.didUpdate($.prevState, $.currentState) >> refresh
      }
      .build

  def make(
    initialValue: A,
    didUpdate: (A, A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true
  )(render: StateSnapshot[A] => VdomElement) = Component(
    Props(initialValue, didUpdate, shouldRefresh, render)
  )

  import org.scalajs.dom
  import io.circe.syntax._

  def syncedWithStorage(
    storage: dom.raw.Storage,
    key: String,
    defaultValue: A,
    didUpdate: (A, A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true
  )(render: StateSnapshot[A] => VdomElement)(implicit enc: Encoder[A], dec: Decoder[A]) = {
    val initialValue  = Helpers.decodeOptionallyFromStorage(storage, key).getOrElse(defaultValue)
    val storeValue    = (a: A) => Callback(storage.setItem(key, a.asJson.noSpaces))
    val didUpdateFull = (p: A, c: A) => didUpdate(p, c) >> storeValue(c)
    Component(Props(initialValue, didUpdateFull, shouldRefresh, render))
  }

  def syncedWithLocalStorage(
    key: String,
    defaultValue: A,
    didUpdate: (A, A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true
  )(render: StateSnapshot[A] => VdomElement)(implicit enc: Encoder[A], dec: Decoder[A]) =
    syncedWithStorage(dom.window.localStorage, key, defaultValue, didUpdate, shouldRefresh)(render)

  def syncedWithSessionStorage(
    key: String,
    defaultValue: A,
    didUpdate: (A, A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true
  )(render: StateSnapshot[A] => VdomElement)(implicit enc: Encoder[A], dec: Decoder[A]) =
    syncedWithStorage(dom.window.sessionStorage, key, defaultValue, didUpdate, shouldRefresh)(
      render
    )
}
