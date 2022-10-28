package livechat

import scala.scalajs.js

import cats.Monoid
import cats.Order
import cats.implicits._

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.CallbackTo

trait PackagePlatformExtensions {

  implicit val callbackMonoid = new Monoid[Callback] {
    override def empty: Callback = Callback.empty
    override def combine(x: Callback, y: Callback): Callback = x >> y
  }
}
