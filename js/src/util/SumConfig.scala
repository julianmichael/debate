package debate
package util

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.MonocleReact._

import scalacss.ScalaCssReact._


import monocle.Prism
import jjm.ui.LocalState

/** HOC middleman for sum types producing a config panel controlled with a
  * drop-down. You need to provide a list of prisms to correspond to the
  * drop-down options. (So I guess this may be more general than just sum types
  * actually, as the prisms can overlap.)
  */
case class SumConfig[A]() {

  val S = debate.Styles
  val V = new jjm.ui.View(S)
  val LocalString = new LocalState[String]

  def mod(
      div: TagMod = S.sumConfigOuterDiv,
      // innerDiv: TagMod = S.sumConfigInnerDiv,
      select: TagMod = S.sumConfigSelect
  )(item: StateSnapshot[A])(
      options: (String, SumConfigOption[A])*
  ) = {
    val initialValue = options
      .flatMap { case (label, option) =>
        option.prism.getOption(item.value).map(_ => label)
      }
      .headOption
      .getOrElse(options.head._1)
    val optionsMap = options.toMap
    LocalString.make(initialValue) { optionName =>
      <.div(div)(
        V.Select.String.modFull(select)(
          options.map(_._1).toList,
          optionName.value,
          choice =>
            optionName.setState(choice) >> item.setState {
              val option = optionsMap(choice)
              val projectedDefault = option.prism.apply(option.default)
              projectedDefault
            }
        ),
        options
          .find(_._1 == optionName.value)
          .map(_._2)
          .flatMap { option =>
            item.zoomStateO(option.prism.asOptional).map { subItem =>
              option.render(subItem)
            }
          }
          .whenDefined
      )
    }
  }

  def apply(item: StateSnapshot[A])(
      options: (String, SumConfigOption[A])*
  ) = mod()(item)(options: _*)
}
sealed trait SumConfigOption[A] {
  type Subtype
  def default: Subtype
  def prism: Prism[A, Subtype]
  def render: StateSnapshot[Subtype] => VdomArray
}
object SumConfigOption {
  private[this] case class SumConfigOptionImpl[A, S](
      default: S,
      prism: Prism[A, S],
      render: StateSnapshot[S] => VdomArray
  ) extends SumConfigOption[A] {
    type Subtype = S
  }
  def apply[A, S](default: S, prism: Prism[A, S])(
      render: StateSnapshot[S] => VdomArray
  ): SumConfigOption[A] { type Subtype = S } =
    SumConfigOptionImpl(default, prism, render)
}
