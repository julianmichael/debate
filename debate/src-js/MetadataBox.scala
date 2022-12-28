package debate

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scalacss.ScalaCssReact._

import cats.implicits._
import japgolly.scalajs.react.feature.ReactFragment
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
        case WaitingToBegin =>
          S.waitingToBeginStatusLabel
        case InProgress =>
          S.inProgressStatusLabel
        case Complete =>
          S.completeStatusLabel
      }
    }

    val statusDisplay =
      <.div(S.optionTitle)(roomMetadata.name, " ", <.span(statusStyle)(s"(${roomMetadata.status})"))

    val turnDisplay = {
      val speakers = roomMetadata.currentSpeakers
      // TODO: this relies on role assignments being unique (injective map).
      // We need to enforce this at debate creation time and (ideally) in the types.
      val myRole = roomMetadata.roleAssignments.map(_.swap).get(userName.value)
      val speakerElements = speakers.collect {
        case (speaker: DebateRole) if roomMetadata.roleAssignments.contains(speaker) =>
          val isMyTurn    = myRole.map(_ == speaker).getOrElse(false)
          val speakerName = roomMetadata.roleAssignments(speaker)
          <.span(
            speakerName,
            ^.fontWeight :=
              (if (isMyTurn)
                 "bold"
               else
                 "normal")
          )
      }
      <.div("Turn: ", speakerElements.toVdomArray).when(speakerElements.nonEmpty)
    }

    val roleAssignments = {

      val debaterRoleAssignments = roomMetadata
        .roleAssignments
        .collect { case (k @ Debater(_), v) =>
          k -> v
        }

      ReactFragment(
        roomMetadata
          .roleAssignments
          .get(Judge)
          .map(name => <.div("Judge: ", <.span(S.judgeAssignment)(name))),
        Option(debaterRoleAssignments)
          .filter(_.nonEmpty)
          .map(roles =>
            <.div(
              "Debaters: ",
              Helpers
                .commaSeparatedTags[Vector, (Debater, String)](
                  roles.toVector.sortBy(_._1.answerIndex),
                  getTag = { case (role, name) =>
                    <.span(S.debaterAssignment(role.answerIndex))(name)
                  }
                )
                .toVdomArray
            )
          )
      )
    }

    val presentParticipants = <
      .div(
        "Present: ",
        Helpers.commaSeparatedSpans(roomMetadata.currentParticipants.toList.sorted).toVdomArray
      )
      .when(roomMetadata.currentParticipants.nonEmpty)

    val deleteRoom =
      <.button(c"btn btn-block btn-danger", S.adminOnly)(
        "Delete room",
        ^.onClick ==>
          ((e: ReactMouseEvent) => {
            e.stopPropagation();
            sendToMainChannel(DeleteRoom(isOfficial, roomMetadata.name))
          })
      )

    val enterRoomButton =
      (^.onClick --> enterRoom(ConnectionSpec(isOfficial, roomMetadata.name, userName.value)))
        .when(canEnterRoom)

    val selectableStyle =
      if (canEnterRoom)
        S.simpleSelectable
      else
        S.simpleUnselectable

    <.div(S.metadataBox, S.optionBox, selectableStyle)(
      statusDisplay,
      roleAssignments,
      presentParticipants,
      turnDisplay,
      deleteRoom,
      enterRoomButton
    )
  }
}
