package debate

import cats.Monoid

import japgolly.scalajs.react.Callback

import scalajs.js
import scala.annotation.nowarn

trait PackagePlatformExtensions {

  implicit val callbackMonoid = new Monoid[Callback] {
    override def empty: Callback = Callback.empty
    override def combine(x: Callback, y: Callback): Callback = x >> y
  }

  import facades.Cookies
  @nowarn("msg=are unrelated")
  def getCookie(cookieId: String) = {
    if(Cookies == js.undefined) {
      System.err.println("For some reason, js-cookie isn't properly linking. Cannot figure out why. Relying on manually downloaded JS blob (see Page.scala).")
      js.Dynamic.global.Cookies.get(cookieId).asInstanceOf[js.UndefOr[String]].toOption
    } else {
      Cookies.get(cookieId).toOption
    }
  }

  @nowarn("msg=are unrelated")
  def setCookie(cookieId: String, message: String, expires: Int) =
    if(Cookies == js.undefined) {
      System.err.println("For some reason, js-cookie isn't properly linking. Cannot figure out why. Relying on manually downloaded JS blob (see Page.scala).")
      js.Dynamic.global.Cookies.set(cookieId, message, js.Dynamic.literal(expires = expires))
    } else {
      Cookies.set(cookieId, message, js.Dynamic.literal(expires = expires))
    }
}
