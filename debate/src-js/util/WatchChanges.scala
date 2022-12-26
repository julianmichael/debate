package debate
package util

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.duration.FiniteDuration
import monocle.macros.Lenses

class WatchChanges[Target, Change](getChanges: (Target, Target) => Vector[Change]) {

  @Lenses
  case class State(prevTarget: Target, recentChanges: Vector[Change])

  type Context = Vector[Change]

  case class Props(
    watchedValue: Target,
    timeToKeepChanges: FiniteDuration,
    render: Context => VdomElement
  )

  val Component =
    ScalaComponent
      .builder[Props]("Watched Changes")
      .getDerivedStateFromPropsAndState[State] { (props, stateOpt) =>
        println("========== GetDerivedStateFromProps ==========")
        println(s"State: $stateOpt")
        println(s"Props: ${props.watchedValue}")

        stateOpt.fold(State(props.watchedValue, Vector())) { state =>
          val changes = getChanges(state.prevTarget, props.watchedValue)
          println(s"Changes: $changes")
          State(props.watchedValue, state.recentChanges ++ changes)
        }
      }
      .render($ => $.props.render($.state.recentChanges))
      .componentDidUpdate { $ =>
        // val changes = getChanges($.prevProps.watchedValue, $.currentProps.watchedValue)
        // println("---------- DidUpdate ----------")
        // println(s"Prev props: ${$.prevProps.watchedValue}")
        // println(s"Current props: ${$.currentProps.watchedValue}")
        // println(s"Prev state: ${$.prevState}")
        // println(s"Current state: ${$.currentState}")
        val numChangesAdded = $.currentState.recentChanges.size - $.prevState.recentChanges.size
        if (numChangesAdded <= 0)
          Callback.empty
        else {
          AsyncCallback
            .unit
            .delay($.currentProps.timeToKeepChanges)
            .completeWith(_ => $.modState(State.recentChanges.modify(_.drop(numChangesAdded))))
        }
      }
      .componentDidMount($ => Callback(s"Mounted: ${$.props.watchedValue}"))
      .componentWillUnmount($ => Callback(s"Unmounted: ${$.props.watchedValue}"))
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
