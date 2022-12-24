package debate

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import cats.Monoid

import japgolly.scalajs.react.Callback
import org.scalajs.jquery.JQueryStatic
@js.native
@JSImport("jquery", JSImport.Namespace)
object jQuery extends JQueryStatic

trait PackagePlatformExtensions {

  implicit val callbackMonoid =
    new Monoid[Callback] {
      override def empty: Callback                             = Callback.empty
      override def combine(x: Callback, y: Callback): Callback = x >> y
    }

}
