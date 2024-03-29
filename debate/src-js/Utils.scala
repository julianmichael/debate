package debate

import cats.Foldable
import cats.Functor
import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._

// mish mash of stuff to be organized later when there's enough to warrant it.
trait UtilsPlatformExtensions {

  implicit class ClassSetInterpolator(val sc: StringContext) {
    def c(args: Any*) =
      // concatenate everything: use the built-in S method (which happens to be used in the S interpolator)
      ^.classSet1(sc.s(args: _*))
  }

  // inclusive
  def clamp(min: Int, value: Int, max: Int): Int = math.min(max, math.max(min, value))

  def tagDelimitedElements[F[_]: Foldable: Functor, A](
    fa: F[A],
    getElement: A => VdomElement,
    delimiter: VdomTag
  ) = fa.map(x => Vector(getElement(x))).intercalate(Vector(delimiter))

  def tagDelimitedTags[F[_]: Foldable: Functor, A](
    fa: F[A],
    getTag: A => VdomTag,
    delimiter: VdomTag,
    getKey: Int => Option[String] = i => Some(s"$i")
  ) = fa
    .map(x => Vector(getTag(x)))
    .intercalate(Vector(delimiter))
    .zipWithIndex
    .map { case (x, i) =>
      x(getKey(i).whenDefined(^.key := _))
    }

  def delimitedTags[F[_]: Foldable: Functor, A](
    fa: F[A],
    getTag: A => VdomTag,
    delimiter: String = ", ",
    getKey: Int => Option[String] = i => Some(s"$i")
  ) = tagDelimitedTags(fa, getTag, <.span(delimiter), getKey)

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

  private val S = Styles
  private val V = new jjm.ui.View(S)

  def textInput(
    field: StateSnapshot[String],
    placeholderOpt: Option[String],
    enter: Callback,
    inputMod: TagMod = TagMod.empty
  ) =
    V.LiveTextField
      .String
      .modInput(input =
        TagMod(
          inputMod,
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
    buttonContent: TagMod,
    isEnabled: Boolean,
    enter: Callback,
    inputMod: TagMod = TagMod.empty
  ) =
    <.div(c"input-group", ^.width.auto)(
      textInput(field = field, placeholderOpt = placeholderOpt, enter = enter, inputMod),
      <.div(c"input-group-append")(
        <.button(c"btn btn-primary")(
          buttonContent,
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

  case class ProbabilityBarItem(probability: Double, barMod: TagMod)
  def probabilityBar(mod: TagMod, items: Vector[ProbabilityBarItem]) =
    <.div(mod, S.judgmentBar)(
      items
        .zipWithIndex
        .map { case (ProbabilityBarItem(prob, bar), index) =>
          val pct       = prob * 100.0
          val pctString = f"$pct%.0f%%"
          <.div(bar, ^.width := pctString, ^.key := s"dist-$index")(<.span(c"ml-1")(pctString))
        }
        .toVdomArray
    )

}
