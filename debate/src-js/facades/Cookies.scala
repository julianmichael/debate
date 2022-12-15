package debate.facades

import scalajs.js
import js.annotation.JSImport
import scala.annotation.nowarn

@nowarn("msg=is never used")
@js.native
@JSImport("js-cookie", "Cookies", "Cookies")
object Cookies extends js.Any {
    def get(name: String): js.UndefOr[String] = js.native
    def set(name: String, value: String, attributes: js.Object): js.UndefOr[String] = js.native
}