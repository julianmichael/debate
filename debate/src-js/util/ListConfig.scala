package debate
package util

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.implicits._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.feature.ReactFragment

/** HOC middleman for easily rendering a config panel for a list of things.
  * Gives add/remove buttons and list format while letting the caller render the
  * list items.
  */
case class ListConfig[A]() {

  import ListConfig.Context

  val S = debate.Styles

  import Helpers.ClassSetInterpolator

  def nice(items: StateSnapshot[Vector[A]], defaultItem: A, minItems: Int)(
    renderItem: Context[A] => VdomElement
  ) = ReactFragment(
    apply(items, minItems) { case context @ Context(_, index, swapUpOpt, removeOpt, swapDownOpt) =>
      val sideButtonStyle = TagMod(c"btn border-0 rounded-0", S.col, S.grow)
      val leftSideButtons =
        Vector(
          swapUpOpt.map(swapUp =>
            <.div(sideButtonStyle, c"btn-outline-secondary")(
              <.div(^.margin.auto, <.i(c"bi bi-arrow-up")),
              ^.onClick --> swapUp
            )
          ),
          removeOpt.map(remove =>
            <.div(sideButtonStyle, c"btn-outline-danger")(
              <.div(^.margin.auto, <.i(c"bi bi-x")),
              ^.onClick --> remove
            )
          ),
          swapDownOpt.map(swapDown =>
            <.div(sideButtonStyle, c"btn-outline-secondary")(
              <.div(^.margin.auto, <.i(c"bi bi-arrow-down")),
              ^.onClick --> swapDown
            )
          )
        ).flatten
      <.div(^.key := s"item-$index")(c"card mb-1", ^.overflow.hidden)(
        <.div(c"row no-gutters")(
          <.div(S.cardLeftSideXColumn)(leftSideButtons: _*).when(leftSideButtons.nonEmpty),
          <.div(c"col")(renderItem(context))
        )
      )
    }.toVdomArray,
    <.button(c"btn btn-outline-secondary")("+", ^.onClick --> items.modState(_ :+ defaultItem))
  )

  def apply[B](values: StateSnapshot[Vector[A]], minItems: Int = 0)(renderItem: Context[A] => B) =
    values
      .value
      .zipWithIndex
      .map { case (_, index) =>
        // safe since we're in zipWithIndex
        val itemSnapshot = values.zoomStateO(Optics.index(index)).get
        val swapUpCb =
          if (index > 0)
            Some(
              values.modState(vs => vs.updated(index, vs(index - 1)).updated(index - 1, vs(index)))
            )
          else
            None
        val swapDownCb =
          if (index < values.value.size - 1) {
            Some(
              values.modState(vs => vs.updated(index, vs(index + 1)).updated(index + 1, vs(index)))
            )
          } else
            None
        val removeItemCb = Option(values.modState(_.remove(index)))
          .filter(_ => values.value.size > minItems)

        renderItem(Context(itemSnapshot, index, swapUpCb, removeItemCb, swapDownCb))
      }
}
object ListConfig {

  case class Context[A](
    item: StateSnapshot[A],
    index: Int,
    swapUp: Option[Callback],
    remove: Option[Callback],
    swapDown: Option[Callback]
  )

  val String = ListConfig[String]()
}
