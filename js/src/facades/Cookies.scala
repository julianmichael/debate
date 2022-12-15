package debate
package facades

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@nowarn("msg=is never used")
@js.native
@JSImport("js-cookie", JSImport.Default)
object Cookies extends js.Object {
  def get(name: String): js.UndefOr[String] = js.native

  def set(name: String, value: String, attributes: js.Object): js.UndefOr[String] = js.native
}
