package debate
package util

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.implicits._
import japgolly.scalajs.react.Callback

/** HOC middleman for easily rendering a config panel for a list of things.
  * Gives add/remove buttons and list format while letting the caller render the
  * list items.
  */
case class ListConfig[A]() {

  import ListConfig.Context

  val S = debate.Styles

  import Helpers.ClassSetInterpolator
  def default(values: StateSnapshot[Vector[A]], defaultItem: A, minItems: Int = 0)(
    renderItem: (TagMod, StateSnapshot[A], Int) => VdomTag
  ) =
    <.div(S.listConfigListDiv)(
      apply(values, minItems) { case Context(item, index, _, removeOpt, _) =>
        val removeItemElement = removeOpt
          .map(remove => <.span(<.i(c"bi bi-x"), ^.onClick --> remove))
        renderItem(removeItemElement, item, index)
      },
      <.div(S.listConfigAddItemDiv)(<.span("(+)", ^.onClick --> values.modState(_ :+ defaultItem)))
    )

  def apply(values: StateSnapshot[Vector[A]], minItems: Int = 0)(
    renderItem: Context[A] => VdomTag
  ) = values
    .value
    .zipWithIndex
    .toVdomArray { case (_, index) =>
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

      renderItem(Context(itemSnapshot, index, swapUpCb, removeItemCb, swapDownCb))(
        ^.key := s"item-$index"
      )
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
