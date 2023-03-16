package debate
package util

import cats.Order
import cats.implicits._

import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

/** HOC middleman for easily rendering a config panel for a list of things.
  * Gives add/remove buttons and list format while letting the caller render the
  * list items.
  */
case class SetConfig[A: Order](show: A => String) {

  import SetConfig.Context

  val S             = debate.Styles
  val V             = new jjm.ui.View(S)
  val NewItemSelect = V.OptionalSelect[A](show, "Choose...")

  import Utils.ClassSetInterpolator

  def nice(choices: Set[A], items: StateSnapshot[Set[A]], minItems: Int)(
    renderItem: Context[A] => VdomElement
  ): VdomElement = {
    val sideButtonStyle = TagMod(c"btn border-0 rounded-0", S.col, S.grow)
    ReactFragment(
      items
        .value
        .map { case item =>
          val context = new Context(item, minItems, items)
          val leftSideButtons =
            Vector(
              context
                .remove
                .map(remove =>
                  <.div(sideButtonStyle, c"btn-outline-danger")(
                    <.div(^.margin.auto, <.i(c"bi bi-x")),
                    ^.onClick --> remove
                  )
                )
            ).flatten
          <.div(^.key := s"item-$item")(c"card mb-1", ^.overflow.hidden)(
            <.div(c"row no-gutters")(
              <.div(S.cardLeftSideXColumn)(leftSideButtons: _*).when(leftSideButtons.nonEmpty),
              <.div(c"col")(renderItem(context))
            )
          )
        }
        .toVdomArray,
      <.div(c"input-group mb-1")(
        <.div(c"input-group-prepend")(<.label(c"input-group-text")(<.i(c"bi bi-plus"))),
        // <.div(S.cardLeftSideXColumn, c"input-group-prepend")(
        //   <.div(sideButtonStyle, c"btn-primary")(<.div(^.margin.auto, <.i(c"bi bi-plus")))
        // ),
        NewItemSelect(
          choices = choices,
          curChoice = None,
          setChoice = _.foldMap(c => items.modState(_ + c))
        )
      )
    )
  }

//   def apply[B](choices: Set[A], items: StateSnapshot[Set[A]], minItems: Int = 0)(
//     renderItem: Context[A] => B
//   ) = items
//     .value
//     .map { item =>
//       renderItem(new Context(item, minItems, items))
//     }
}
object SetConfig {

  object Context {
    def unapply[A](c: Context[A]) = Some(c.item)
  }
  class Context[A](val item: A, minItems: Int, items: StateSnapshot[Set[A]]) {
    private def numItems = items.value.size

    def remove = Option(items.modState(_ - item)).filter(_ => numItems > minItems)
  }

  val String = SetConfig[String](identity)
}
