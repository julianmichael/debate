package debate

import cats.Foldable
import cats.Functor
import cats.implicits._

import io.circe.generic.JsonCodec
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

@JsonCodec
case class ConnectionSpec(isOfficial: Boolean, roomName: String, participantName: String)

object Helpers {

  implicit class ClassSetInterpolator(val sc: StringContext) extends AnyVal {
    def c(args: Any*) =
      // concatenate everything: use the built-in S method (which happens to be used in the S interpolator)
      ^.classSet1(sc.s(args: _*))
  }

  def commaSeparatedSpans[F[_]: Foldable: Functor](
    fa: F[String],
    getKey: (String, Int) => Option[String] = (x, i) => Some(s"$i-$x")
  ) = fa
    .map(Vector(_))
    .intercalate(Vector(", "))
    .zipWithIndex
    .map { case (x, i) =>
      <.span(x, getKey(x, i).whenDefined(^.key := _))
    }

  def wsProtocol =
    if (dom.document.location.protocol == "https:")
      "wss:"
    else
      "ws:"

  def makePageTitle(x: String) = s"$x | Debate"
}
