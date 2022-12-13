package debate.util

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.ReactMonocle._

import scalacss.ScalaCssReact._

import monocle.function.{all => Optics}

import jjm.implicits._

/** HOC middleman for easily rendering a config panel for a list of things.
  * Gives add/remove buttons and list format while letting the caller render the
  * list items.
  */
case class ListConfig[A](defaultItem: A) {

  val S = debate.Styles

  def mod(
      listDiv: TagMod = S.listConfigListDiv,
      removeItemSpan: TagMod = S.listConfigRemoveItemSpan,
      addItemDiv: TagMod = S.listConfigAddItemDiv,
      addItemSpan: TagMod = S.listConfigAddItemSpan
  )(values: StateSnapshot[Vector[A]], minItems: Int = 0)(
      renderItem: (TagMod, StateSnapshot[A], Int) => VdomTag
  ) = <.div(listDiv)(
    values.value.zipWithIndex.toVdomArray { case (_, index) =>
      // safe since we're in zipWithIndex
      val itemSnapshot = values.zoomStateO(Optics.index(index)).get
      val removeItemElement = <.span(removeItemSpan)(
        "(-)",
        ^.onClick --> values.modState(_.remove(index))
      ).when(values.value.size > minItems)

      renderItem(removeItemElement, itemSnapshot, index)(
        ^.key := s"item-$index"
      )
    },
    <.div(addItemDiv)(
      <.span(addItemSpan)(
        "(+)",
        ^.onClick --> values.modState(_ :+ defaultItem)
      )
    )
  )

  def apply(values: StateSnapshot[Vector[A]], minItems: Int = 0)(
      renderItem: (TagMod, StateSnapshot[A], Int) => VdomTag
  ) = mod()(values, minItems)(renderItem)
}
object ListConfig {
  // def Double = LiveTextField[Double]((s: String) => scala.util.Try(s.toDouble).toOption, _.toString)
  val String = ListConfig[String]("")
}
