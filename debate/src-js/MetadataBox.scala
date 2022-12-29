package debate

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scalacss.ScalaCssReact._

import jjm.implicits._

import cats.implicits._
import japgolly.scalajs.react.feature.ReactFragment
import jjm.ui.Rgba
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

    case class OverUnder(label: VdomNode, bgStyle: TagMod)

    val resultOverUnderOpt = roomMetadata
      .result
      .map { result =>
        val correctConfidence = result.finalJudgement(result.correctAnswerIndex) * 100
        val otherConfidences = result
          .finalJudgement
          .remove(result.correctAnswerIndex)
          .sortBy(-_)
          .map(_ * 100)

        val label = <.span(
          <.span(S.correct)(f"$correctConfidence%.0f%%"),
          "/",
          Helpers
            .delimitedTags[Vector, Double](
              otherConfidences,
              getTag = conf => <.span(S.incorrect)(f"$conf%.0f%%"),
              delimiter = "/"
            )
            .toVdomArray
        )

        val style =
          otherConfidences.headOption match {
            case None =>
              TagMod.empty
            case Some(secondGuessConfidence) =>
              val correctnessScore = correctConfidence / (correctConfidence + secondGuessConfidence)
              val opacity          = math.abs(correctnessScore - 0.5)
              val color =
                if (correctnessScore > 0.5)
                  Rgba(0, 128, 0, opacity) // green
                else
                  Rgba(220, 20, 60, opacity) // crimson
              TagMod(^.backgroundColor := color.toColorStyleString)
          }

        OverUnder(label, style)
      }

    val boxTitle =
      <.div(S.optionTitle)(
        roomMetadata.name,
        resultOverUnderOpt.map(overUnder => <.span(" (", overUnder.label, ")"))
      )

    val storyTitle = <.div("Story: ", <.i(roomMetadata.storyTitle))

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
          .map(name =>
            <.div(
              "Judge: ",
              <.span(
                S.judgeAssignment,
                ^.fontWeight.bold.when(roomMetadata.currentParticipants.contains(name))
              )(name)
            )
          ),
        Option(debaterRoleAssignments)
          .filter(_.nonEmpty)
          .map(roles =>
            <.div(
              "Debaters: ",
              Helpers
                .delimitedTags[Vector, (Debater, String)](
                  roles.toVector.sortBy(_._1.answerIndex),
                  getTag = { case (role, name) =>
                    <.span(
                      S.debaterAssignment(role.answerIndex),
                      ^.fontWeight.bold.when(roomMetadata.currentParticipants.contains(name))
                    )(name)
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
        Helpers.delimitedSpans(roomMetadata.currentParticipants.toList.sorted).toVdomArray
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

    <.div(S.metadataBox, S.optionBox, selectableStyle, resultOverUnderOpt.whenDefined(_.bgStyle))(
      boxTitle,
      storyTitle,
      roleAssignments,
      presentParticipants,
      turnDisplay,
      deleteRoom,
      enterRoomButton
    )
  }
}
