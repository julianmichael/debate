package debate
package util

import io.circe.Decoder
import io.circe.Encoder
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scala.reflect.ClassTag

class Local[A] private (name: String) {
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
      .builder[Props](s"Local $name")
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
  )(render: StateSnapshot[A] => VdomElement): VdomElement = Component(
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
  )(
    render: StateSnapshot[A] => VdomElement
  )(implicit enc: Encoder[A], dec: Decoder[A]): VdomElement = {
    val initialValue  = Utils.decodeOptionallyFromStorage(storage, key).getOrElse(defaultValue)
    val storeValue    = (a: A) => Callback(storage.setItem(key, a.asJson.noSpaces))
    val didUpdateFull = (p: A, c: A) => didUpdate(p, c) >> storeValue(c)
    Component(Props(initialValue, didUpdateFull, shouldRefresh, render))
  }

  def syncedWithLocalStorage(
    key: String,
    defaultValue: A,
    didUpdate: (A, A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true
  )(
    render: StateSnapshot[A] => VdomElement
  )(implicit enc: Encoder[A], dec: Decoder[A]): VdomElement =
    syncedWithStorage(dom.window.localStorage, key, defaultValue, didUpdate, shouldRefresh)(render)

  def syncedWithSessionStorage(
    key: String,
    defaultValue: A,
    didUpdate: (A, A) => Callback = (_, _) => Callback.empty,
    shouldRefresh: A => Boolean = (_: A) => true
  )(
    render: StateSnapshot[A] => VdomElement
  )(implicit enc: Encoder[A], dec: Decoder[A]): VdomElement =
    syncedWithStorage(dom.window.sessionStorage, key, defaultValue, didUpdate, shouldRefresh)(
      render
    )
}
object Local {
  var instances = new collection.mutable.HashMap[String, Local[_]]

  def apply[A](implicit ct: ClassTag[A]): Local[A] = {
    val name = ct.runtimeClass.getName()
    instances.get(name) match {
      case None =>
        val instance = new Local[A](name)
        instances.put(name, instance)
        instance
      case Some(instance) =>
        instance.asInstanceOf[Local[A]]
    }
  }

  def named[A](name: String)(implicit ct: ClassTag[A]): Local[A] = {
    val fullName = s"$name: " + ct.runtimeClass.getName()
    instances.get(fullName) match {
      case None =>
        val instance = new Local[A](fullName)
        instances.put(name, instance)
        instance
      case Some(instance) =>
        instance.asInstanceOf[Local[A]]
    }
  }

}
