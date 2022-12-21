package debate

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scalacss.ScalaCssReact._

import cats.implicits._

case class Props(
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
) {
  import Helpers.ClassSetInterpolator

  val S = Styles
  def canEnterRoom = {
    userName.value.nonEmpty && !roomMetadata.currentParticipants
      .contains(userName.value)
  }

  def statusDisplay = {
    <.div(S.optionTitle)(
      roomMetadata.name
    )
  }

  def assignedParticipants = {
    <.div(
      <.strong("Assigned: "),
      Helpers
        .commaSeparatedSpans(
          roomMetadata.assignedParticipants.toList.sorted
        )
        .toVdomArray
    ).when(roomMetadata.assignedParticipants.nonEmpty)
  }

  def presentParticipants = {
    <.div(
      <.strong("Present: "),
      Helpers
        .commaSeparatedSpans(
          roomMetadata.currentParticipants.toList.sorted
        )
        .toVdomArray
    ).when(roomMetadata.currentParticipants.nonEmpty)
  }

  def deleteRoom = {
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

  def enterRoomButton = {
    (^.onClick --> enterRoom(
      isOfficial,
      roomMetadata.name,
      userName.value
    )).when(canEnterRoom)
  }
}

object DebateMetadata {
  val S = Styles

  // TODO refactor to use props and a class?

  def make(
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

    val props = Props(
      roomMetadata,
      isOfficial,
      userName,
      enterRoom,
      sendToMainChannel
    )
    val selectableStyle =
      if (props.canEnterRoom) S.simpleSelectable
      else S.simpleUnselectable
    <.div(S.optionBox, selectableStyle)(
      props.statusDisplay,
      props.assignedParticipants,
      props.presentParticipants,
      props.deleteRoom,
      props.enterRoomButton
    )
  }
}
