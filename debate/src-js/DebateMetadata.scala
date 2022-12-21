package debate

import scala.language.existentials // see https://github.com/suzaku-io/diode/issues/50

import org.scalajs.dom

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scalacss.ScalaCssReact._

import cats.implicits._
object DebateMetadata {
  import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)
  // TODO refactor
  def roomManagement(
      roomMetadata: RoomMetadata,
      isOfficial: Boolean,
      userName: StateSnapshot[String],
      enterRoom: (
          Boolean,
          String,
          String
      ) => CallbackTo[Unit],
      sendToMainChannel: debate.MainChannelRequest => japgolly.scalajs.react.CallbackTo[
        Unit
      ]
  ) = {
    val canEnterRoom =
      userName.value.nonEmpty && !roomMetadata.currentParticipants
        .contains(userName.value)
    val statusStyle = {
      import RoomStatus._
      roomMetadata.status match {
        case SettingUp => S.settingUpStatusLabel
        case InProgress =>
          S.inProgressStatusLabel
        case Complete => S.completeStatusLabel
      }
    }

    // TODO probably just a val instead of a def
    def statusDisplay(status: debate.RoomStatus) = {
      <.div(S.optionTitle)(
        roomMetadata.name,
        " ",
        <.span(statusStyle)(s"($status)")
      )
    }

    def assignedParticipants() = {
      <.div(
        <.strong("Assigned: "),
        Helpers
          .commaSeparatedSpans(
            roomMetadata.assignedParticipants.toList.sorted
          )
          .toVdomArray
      ).when(roomMetadata.assignedParticipants.nonEmpty)
    }

    def presentParticipants() = {
      <.div(
        <.strong("Present: "),
        Helpers
          .commaSeparatedSpans(
            roomMetadata.currentParticipants.toList.sorted
          )
          .toVdomArray
      ).when(roomMetadata.currentParticipants.nonEmpty)
    }

    def deleteRoom() = {
      <.button(
        c"btn btn-block btn-danger",
        S.adminOnly
      )(
        "Delete room",
        ^.onClick ==> ((e: ReactMouseEvent) => {
          e.stopPropagation();
          sendToMainChannel(
            DeleteRoom(isOfficial, roomMetadata.name)
          )
        })
      )
    }

    def enterRoomButton() = {
      (^.onClick --> enterRoom(
        isOfficial,
        roomMetadata.name,
        userName.value
      )).when(canEnterRoom)
    }

    val selectableStyle =
      if (canEnterRoom) S.simpleSelectable
      else S.simpleUnselectable
    val status = roomMetadata.status
    <.div(S.optionBox, selectableStyle)(
      statusDisplay(status = status),
      assignedParticipants(),
      presentParticipants(),
      deleteRoom(),
      enterRoomButton()
    )
  }
}
