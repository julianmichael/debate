package livechat

import scala.scalajs.js

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.MonocleReact._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import cats.Order
import cats.implicits._

import monocle.macros._

import org.scalajs.dom.experimental.mediastream.MediaDeviceKind

object View {

  def liveSlider(
    value: StateSnapshot[Double],
    min: Double,
    max: Double,
    step: Double,
    fromValue: Double => Double,
    toValue: Double => Double,
    onChange: Double => Callback,
  ) = {
    <.input(
      ^.`type` := "range",
      ^.min := min,
      ^.max := max,
      ^.step := step,
      ^.value := fromValue(value.value),
      ^.onChange ==> (
        (e: ReactEventFromInput) => {
          val newValue = toValue(e.target.value.toDouble)
          value.setState(newValue) >> onChange(newValue)
        })
    )
  }

  class OptionalSelect[A: Order](
    show: A => String,
    none: String = "None") {

    def apply(
      choices: Set[A],
      curChoice: Option[A],
      setChoice: Option[A] => Callback
    ) = <.select(
      ^.value := curChoice.fold(none)(show),
      ^.onChange ==> (
        (e: ReactEventFrom[org.scalajs.dom.html.Select]) => {
          val valStr = e.target.value
          val value = {
            if(valStr == none) None
            else choices.find(c => show(c) == valStr)
          }
          if(value != curChoice) setChoice(value) else Callback.empty
        }
      ),
      <.option(^.key := none, ^.value := none, none),
      choices.toList.sorted.map(show).zipWithIndex.toVdomArray { case (c, i) =>
        <.option(^.key := s"$c-$i", ^.value := c, c)
      }
    )
  }
}
