package debate
package util

import scala.reflect.ClassTag

import cats.implicits._

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Optional

import jjm.ui.Mounting

object PartialStateBackup {
  def apply[State, Part: ClassTag](
    state: StateSnapshot[State],
    initialPartValue: Part,
    partOptional: Optional[State, Part]
  )(render: (StateSnapshot[Part], Option[StateSnapshot[Part]]) => VdomElement) =
    Local[Part].make(initialPartValue) { storedPart =>
      val syncedPartOpt: Option[StateSnapshot[Part]] = state
        .zoomStateO(partOptional)
        .map { partState =>
          StateSnapshot(partState.value)((sOpt, cb) =>
            partState.setStateOption(sOpt, storedPart.setStateOption(sOpt, cb))
          )
        }
      Mounting.make(partOptional.getOption(state.value).foldMap(storedPart.setState(_)))(
        render(storedPart, syncedPartOpt)
      )
    }
}
