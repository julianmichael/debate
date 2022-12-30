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

  def default(values: StateSnapshot[Vector[A]], defaultItem: A, minItems: Int = 0)(
    renderItem: (TagMod, StateSnapshot[A], Int) => VdomTag
  ) =
    <.div(S.listConfigListDiv)(
      apply(values, minItems) { case Context(item, index, removeCallback) =>
        val removeItemElement = removeCallback
          .map(remove => <.span(S.listConfigRemoveItemSpan)("(-)", ^.onClick --> remove))
        renderItem(removeItemElement, item, index)
      },
      <.div(S.listConfigAddItemDiv)(
        <.span(S.listConfigAddItemSpan)("(+)", ^.onClick --> values.modState(_ :+ defaultItem))
      )
    )

  def apply(values: StateSnapshot[Vector[A]], minItems: Int = 0)(
    renderItem: Context[A] => VdomTag
  ) = values
    .value
    .zipWithIndex
    .toVdomArray { case (_, index) =>
      // safe since we're in zipWithIndex
      val itemSnapshot = values.zoomStateO(Optics.index(index)).get
      val removeItemCallback = Option(values.modState(_.remove(index)))
        .filter(_ => values.value.size > minItems)

      renderItem(Context(itemSnapshot, index, removeItemCallback))(^.key := s"item-$index")
    }
}
object ListConfig {

  case class Context[A](item: StateSnapshot[A], index: Int, remove: Option[Callback])

  val String = ListConfig[String]()
}
