package debate
package util

import scala.reflect.ClassTag

import cats.~>

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.implicits._

/** HOC middleman for easily rendering a config panel for a list of things.
  * Gives add/remove buttons and list format while letting the caller render the
  * list items.
  */
class ListConfig[A] {

  import ListConfig.Context

  val S = debate.Styles

  import Utils.ClassSetInterpolator

  def nice(
    items: StateSnapshot[Vector[A]],
    defaultItem: A,
    minItems: Int,
    includeAddButton: Boolean = true,
    hideDeleteButtons: Boolean = false
  )(renderItem: Context[A] => VdomElement): VdomElement =
    nice(
      items,
      defaultItem,
      minItems,
      includeAddButton,
      hideDeleteButtons,
      f => items.modState(f[A])
    )(renderItem)

  def nice(
    items: StateSnapshot[Vector[A]],
    defaultItem: A,
    minItems: Int,
    // allow for rearrangements to rearrange other stuff too
    includeAddButton: Boolean,
    hideDeleteButtons: Boolean,
    rearrange: Vector ~> Vector => Callback
  )(renderItem: Context[A] => VdomElement): VdomElement = ReactFragment(
    apply(items, minItems)(rearrange) { case context @ Context(_, index) =>
      val sideButtonStyle = TagMod(c"btn border-0 rounded-0", S.col, S.grow)
      val leftSideButtons =
        Vector(
          context
            .swapUp
            .map(swapUp =>
              <.div(sideButtonStyle, c"btn-outline-secondary")(
                <.div(^.margin.auto, <.i(c"bi bi-arrow-up")),
                ^.onClick --> swapUp
              )
            ),
          context
            .remove
            .map(remove =>
              <.div(
                sideButtonStyle,
                c"btn-outline-danger",
                ^.visibility.hidden.when(hideDeleteButtons)
              )(<.div(^.margin.auto, <.i(c"bi bi-x")), ^.onClick --> remove)
            ),
          context
            .swapDown
            .map(swapDown =>
              <.div(sideButtonStyle, c"btn-outline-secondary")(
                <.div(^.margin.auto, <.i(c"bi bi-arrow-down")),
                ^.onClick --> swapDown
              )
            ),
          Option(index)
            .filter(_ == items.value.size - 1 && includeAddButton)
            .map(_ =>
              <.div(sideButtonStyle, c"btn-outline-primary")(
                <.div(^.margin.auto, <.i(c"bi bi-plus")),
                ^.onClick --> items.modState(_ :+ defaultItem)
              )
            )
        ).flatten
      <.div(^.key := s"item-$index")(c"card mb-1", ^.overflow.hidden)(
        <.div(c"row no-gutters")(
          <.div(S.cardLeftSideXColumn)(leftSideButtons: _*).when(leftSideButtons.nonEmpty),
          <.div(c"col", ^.overflow.hidden)(renderItem(context))
        )
      )
    }.toVdomArray,
    Option(items.value.size)
      .filter(_ == 0 && includeAddButton)
      .map(_ =>
        <.button(c"btn btn-block btn-outline-primary")(
          <.div(^.margin.auto, <.i(c"bi bi-plus")),
          ^.onClick --> items.modState(_ :+ defaultItem)
        )
      )
  )

  def apply[B](values: StateSnapshot[Vector[A]], minItems: Int = 0)(
    rearrange: Vector ~> Vector => Callback = f => values.modState(f[A])
  )(renderItem: Context[A] => B) = values
    .value
    .zipWithIndex
    .map { case (_, index) =>
      // safe since we're in zipWithIndex
      val itemSnapshot = values.zoomStateO(Optics.index(index)).get
      renderItem(new Context(itemSnapshot, index, minItems, values, rearrange))
    }
}
object ListConfig {

  object Context {
    def unapply[A](c: Context[A]) = Some((c.item, c.index))
  }
  class Context[A](
    val item: StateSnapshot[A],
    val index: Int,
    minItems: Int,
    items: StateSnapshot[Vector[A]],
    change: (Vector ~> Vector) => Callback
  ) {
    private def numItems = items.value.size

    private def swap(i: Int, j: Int): Callback = change(
      λ[Vector ~> Vector](vs => vs.updated(i, vs(j)).updated(j, vs(i)))
    )

    def maybeSwap(i: Int, j: Int): Option[Callback] =
      if (i >= 0 && j >= 0 && i < numItems && j < numItems && i != j) {
        Some(swap(i, j))
      } else
        None

    def swapUp: Option[Callback]   = maybeSwap(index, index - 1)
    def swapDown: Option[Callback] = maybeSwap(index, index + 1)

    def remove = Option(change(λ[Vector ~> Vector](_.remove(index))))
      .filter(_ => numItems > minItems)

    // def append(item: A) = items.modState(_ :+ item)

    // NOTE: these don't work because the move operates on a stale copy of the state
    // private def move(from: Int, to: Int): Callback = change(
    //   λ[Vector ~> Vector] { vs =>
    //     val removed = vs.remove(from)
    //     removed.take(to) ++ (vs(from) +: removed.drop(to))
    //   }
    // )
    // def addAbove(item: A) = append(item) >> move(numItems, index)
    // def addBelow(item: A) = append(item) >> move(numItems, index + 1)

    // def addBelow(item: A) = change(
    //   λ[Vector ~> Vector](vs => vs.take(index + 1) ++ (item +: vs.drop(index + 1)))
    // )

  }

  // val String = ListConfig[String]()

  var instances = new collection.mutable.HashMap[String, ListConfig[_]]

  def getInstance[A](name: String) =
    instances.get(name) match {
      case None =>
        val instance = new ListConfig[A] // (name)
        instances.put(name, instance)
        instance
      case Some(instance) =>
        instance.asInstanceOf[ListConfig[A]]
    }

  def apply[A](implicit ct: ClassTag[A]): ListConfig[A] = getInstance(ct.runtimeClass.getName())

  // def named[A](name: String)(implicit ct: ClassTag[A]): ListConfig[A] = {
  //   val fullName = s"$name: " + ct.runtimeClass.getName()
  //   getInstance(fullName)
  // }
}
