package debate

import japgolly.scalajs.react.vdom.html_<^._

import cats.Foldable
import cats.Functor
import cats.implicits._
import org.scalajs.dom
import jjm.ling.Span
import jjm.ling.Text

case class ConnectionSpec(
    isOfficial: Boolean,
    roomName: String,
    participantName: String
)

object Helpers {

  implicit class ClassSetInterpolator(val sc: StringContext) extends AnyVal {
    def c(args: Any*) = {
      // concatenate everything: use the built-in S method (which happens to be used in the S interpolator)
      ^.classSet1(sc.s(args: _*))
    }
  }

  def commaSeparatedSpans[F[_]: Foldable: Functor](fa: F[String]) = {
    fa.map(x => Vector(<.span(x))).intercalate(Vector(<.span(", ")))
  }

  // more efficient than the jjm default, which I didn't even realize was a problem haha
  def renderSpan(tokens: Vector[String], span: Span) = {
    Text.render(tokens.slice(span.begin, span.endExclusive))
  }

  def wsProtocol() = {
    if (dom.document.location.protocol == "https:") "wss:" else "ws:"
  }
}
