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
          roomMetadata.assignedParticipants.values.toList.sorted
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

  def boldedTurnDisplay = {
    val speakers = roomMetadata.currentSpeakers.getOrElse(Set())
    val myRole =
      roomMetadata.assignedParticipants.map(_.swap).get(userName.value)
    val speakerElements = speakers collect {
      case (speaker: DebateRole)
          if roomMetadata.assignedParticipants.contains(speaker) => {
        val isMyTurn = myRole.map(_ == speaker).getOrElse(false)
        val speakerName = roomMetadata.assignedParticipants(speaker)
        <.span(
          speakerName,
          ^.fontWeight := (if (isMyTurn) "bold" else "normal")
        )
      }
    }
    <.div(
      <.strong("Turn: "),
      speakerElements.toVdomArray
    ).when(speakerElements.nonEmpty)
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
    <.div(S.optionBox, selectableStyle, S.debateMetadata)(
      props.statusDisplay,
      props.assignedParticipants,
      props.presentParticipants,
      props.boldedTurnDisplay,
      props.deleteRoom,
      props.enterRoomButton
    )
  }
}
