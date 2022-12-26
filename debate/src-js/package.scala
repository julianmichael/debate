package debate

import cats.Monoid

import japgolly.scalajs.react.Callback

trait PackagePlatformExtensions {

  implicit val callbackMonoid =
    new Monoid[Callback] {
      override def empty: Callback                             = Callback.empty
      override def combine(x: Callback, y: Callback): Callback = x >> y
    }

}
