package debate

import cats.Foldable
import cats.Functor
import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.Callback

object Helpers {

  implicit class ClassSetInterpolator(val sc: StringContext) extends AnyVal {
    def c(args: Any*) =
      // concatenate everything: use the built-in S method (which happens to be used in the S interpolator)
      ^.classSet1(sc.s(args: _*))
  }

  // inclusive
  def clamp(min: Int, value: Int, max: Int): Int = math.min(max, math.max(min, value))

  def delimitedTags[F[_]: Foldable: Functor, A](
    fa: F[A],
    getTag: A => VdomTag,
    delimiter: String = ", ",
    getKey: Int => Option[String] = i => Some(s"$i")
  ) = fa
    .map(x => Vector(getTag(x)))
    .intercalate(Vector(<.span(delimiter)))
    .zipWithIndex
    .map { case (x, i) =>
      x(getKey(i).whenDefined(^.key := _))
    }

  def delimitedSpans[F[_]: Foldable: Functor](
    fa: F[String],
    delimiter: String = ", ",
    getKey: Int => Option[String] = i => Some(s"$i")
  ) = delimitedTags(fa, (x: String) => <.span(x), delimiter, getKey)

  def wsProtocol =
    if (dom.document.location.protocol == "https:")
      "wss:"
    else
      "ws:"

  def makePageTitle(x: String) = s"$x | Debate"

  val S = Styles
  val V = new jjm.ui.View(S)

  def textInput(field: StateSnapshot[String], placeholderOpt: Option[String], enter: Callback) =
    V.LiveTextField
      .String
      .modInput(input =
        TagMod(
          c"form-control",
          ^.onKeyDown ==>
            ((e: ReactKeyboardEvent) =>
              if (e.keyCode == dom.ext.KeyCode.Enter)
                enter
              else
                Callback.empty
            )
        )
      )(field, placeholderOpt = placeholderOpt)

  def textInputWithEnterButton(
    field: StateSnapshot[String],
    placeholderOpt: Option[String],
    buttonText: String,
    isEnabled: Boolean,
    enter: Callback
  ) =
    <.div(c"input-group", ^.width.auto)(
      textInput(enter = enter, field = field, placeholderOpt = placeholderOpt),
      <.div(c"input-group-append")(
        <.button(c"btn btn-primary")(
          buttonText,
          ^.`type`   := "button",
          ^.disabled := !isEnabled,
          ^.onClick --> enter
        )
      )
    )

  import io.circe.Decoder
  import io.circe.parser
  def decodeOptionallyFromStorage[A: Decoder](storage: dom.raw.Storage, key: String) = Option(
    storage.getItem(key)
  ).flatMap(str =>
    parser.decode[A](str) match {
      case Right(result) =>
        Some(result)
      case Left(err) =>
        System
          .err
          .println(
            s"Failed to decode LocalState from storage at key $key. " +
              "Printing error stack trace."
          )
        err.printStackTrace()
        None
    }
  )
}
