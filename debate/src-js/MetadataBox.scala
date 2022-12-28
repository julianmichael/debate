package debate

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scalacss.ScalaCssReact._

import cats.implicits._
object MetadataBox {
  import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def apply(
    roomMetadata: RoomMetadata,
    isOfficial: Boolean,
    userName: StateSnapshot[String],
    enterRoom: ConnectionSpec => CallbackTo[Unit],
    sendToMainChannel: debate.MainChannelRequest => japgolly.scalajs.react.CallbackTo[Unit]
  ) = {
    val canEnterRoom =
      userName.value.nonEmpty && !roomMetadata.currentParticipants.contains(userName.value)
    val statusStyle = {
      import RoomStatus._
      roomMetadata.status match {
        case InProgress =>
          S.inProgressStatusLabel
        case Complete =>
          S.completeStatusLabel
      }
    }

    // TODO probably just a val instead of a def
    def statusDisplay(status: RoomStatus) =
      <.div(S.optionTitle)(roomMetadata.name, " ", <.span(statusStyle)(s"($status)"))

    def assignedParticipants() = <
      .div(
        <.strong("Assigned: "),
        Helpers.commaSeparatedSpans(roomMetadata.assignedParticipants.toList.sorted).toVdomArray
      )
      .when(roomMetadata.assignedParticipants.nonEmpty)

    def presentParticipants() = <
      .div(
        <.strong("Present: "),
        Helpers.commaSeparatedSpans(roomMetadata.currentParticipants.toList.sorted).toVdomArray
      )
      .when(roomMetadata.currentParticipants.nonEmpty)

    def deleteRoom() =
      <.button(c"btn btn-block btn-danger", S.adminOnly)(
        "Delete room",
        ^.onClick ==>
          ((e: ReactMouseEvent) => {
            e.stopPropagation();
            sendToMainChannel(DeleteRoom(isOfficial, roomMetadata.name))
          })
      )

    def enterRoomButton() =
      (^.onClick --> enterRoom(ConnectionSpec(isOfficial, roomMetadata.name, userName.value)))
        .when(canEnterRoom)

    val selectableStyle =
      if (canEnterRoom)
        S.simpleSelectable
      else
        S.simpleUnselectable
    val status = roomMetadata.status
    <.div(c"text-center", S.optionBox, selectableStyle)(
      statusDisplay(status = status),
      assignedParticipants(),
      presentParticipants(),
      deleteRoom(),
      enterRoomButton()
    )
  }
}
