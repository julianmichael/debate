package debate


import cats.Monoid

import japgolly.scalajs.react.Callback

import scalajs.js

trait PackagePlatformExtensions {

  implicit val callbackMonoid = new Monoid[Callback] {
    override def empty: Callback = Callback.empty
    override def combine(x: Callback, y: Callback): Callback = x >> y
  }

  def getCookie(cookieId: String) =
      js.Dynamic.global.Cookies.get(cookieId)
        .asInstanceOf[scalajs.js.UndefOr[String]]
        .toOption

    def setCookie(cookieId: String, message: String, expires: Int) =
      js.Dynamic.global.Cookies.set(
        cookieId, message, js.Dynamic.literal(expires = expires)
      )

}
