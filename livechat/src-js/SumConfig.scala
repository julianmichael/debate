package livechat

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle.function.{all => Optics}

import jjm.implicits._
import monocle.Prism
import jjm.ui.LocalState

case class SumConfig[A]() {

  val S = livechat.Styles
  val V = new jjm.ui.View(S)
  val LocalString = new LocalState[String]

  def mod(
    outerSpan: TagMod = S.sumConfigOuterSpan,
    innerSpan: TagMod = S.sumConfigOuterSpan,
    select: TagMod = S.sumConfigSelect)(
    item: StateSnapshot[A])(
    options: (String, SumConfigOption[A])*
  ) = {
    val initialValue = options.flatMap { case (label, option) =>
      option.prism.getOption(item.value).map(_ => label)
    }.headOption.getOrElse(options.head._1)
    val optionsMap = options.toMap
    <.span(outerSpan)(
      LocalString.make(initialValue) { optionName =>
        <.span(innerSpan)(
          V.Select.String.modFull(select)(
            options.map(_._1).toList,
            optionName.value,
            choice => optionName.setState(choice) >> item.setState {
              val option = optionsMap(choice)
              val projectedDefault = option.prism.apply(option.default)
              projectedDefault
            }
          ),
          options.find(_._1 == optionName.value).map(_._2).flatMap { option =>
            item.zoomStateO(option.prism.asOptional).map { subItem =>
              option.render(subItem)
            }
          }.whenDefined
        )
      }
    )
  }

  def apply(
    item: StateSnapshot[A])(
    options: (String, SumConfigOption[A])*,
  ) = mod()(item)(options:_*)
}
sealed trait SumConfigOption[A] {
  type Subtype
  def default: Subtype
  def prism: Prism[A, Subtype]
  def render: StateSnapshot[Subtype] => VdomElement
}
object SumConfigOption {
  private[this] case class SumConfigOptionImpl[A, S](
    default: S,
    prism: Prism[A, S],
    render: StateSnapshot[S] => VdomElement
  ) extends SumConfigOption[A] {
    type Subtype = S
  }
  def apply[A, S](default: S, prism: Prism[A, S])(
    render: StateSnapshot[S] => VdomElement
  ): SumConfigOption[A] { type Subtype = S } = SumConfigOptionImpl(default, prism, render)
}
