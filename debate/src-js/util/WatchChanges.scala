package debate
package util

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.duration.FiniteDuration

class WatchChanges[Target, Change](getChanges: (Target, Target) => Vector[Change]) {

  type State   = Vector[Change]
  type Context = State
  case class Props(
    watchedValue: Target,
    timeToKeepChanges: FiniteDuration,
    render: Context => VdomElement
  )

  val Component =
    ScalaComponent
      .builder[Props]("Watched Changes")
      .initialState(Vector.empty[Change])
      .render { $ =>
        $.props.render($.state)
      }
      .componentDidUpdate { $ =>
        val changes = getChanges($.prevProps.watchedValue, $.currentProps.watchedValue)
        println(s"Prev: ${$.prevProps.watchedValue}")
        println(s"Current: ${$.currentProps.watchedValue}")
        println(s"Changes: $changes")
        if (changes.isEmpty)
          Callback.empty
        else {
          $.modState(_ ++ changes) >>
            AsyncCallback
              .unit
              .delay($.currentProps.timeToKeepChanges)
              .completeWith(_ => $.modState(_.drop(changes.size)))
          // $.modState(_.drop(changes.size)).delay($.currentProps.timeToKeepChanges).toCallback
        }
      }
      .build

  def make(watchedValue: Target, timeToKeepChanges: FiniteDuration)(
    render: Context => VdomElement
  ) = Component(Props(watchedValue, timeToKeepChanges, render))
}
object WatchChanges {
  case class SetChange[A](isAdded: Boolean, item: A)
  def ofSet[A] =
    new WatchChanges[Set[A], SetChange[A]](
      getChanges =
        (set1: Set[A], set2: Set[A]) =>
          (set2 -- set1).view.map(SetChange(true, _)).toVector ++
            (set1 -- set2).view.map(SetChange(false, _)).toVector
    )
}
