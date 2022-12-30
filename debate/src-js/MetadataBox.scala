package debate

import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import jjm.implicits._
import jjm.ui.Rgba

object MetadataBox {
  import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def apply(
    isAdmin: Boolean,
    roomMetadata: RoomMetadata,
    isOfficial: Boolean,
    userName: String,
    enterRoom: ConnectionSpec => CallbackTo[Unit],
    sendToMainChannel: debate.MainChannelRequest => japgolly.scalajs.react.CallbackTo[Unit]
  ) = {
    val canEnterRoom = userName.nonEmpty // &&
    // !roomMetadata.currentParticipants.contains(userName)

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

    def getRoleStyle(role: DebateRole) =
      role match {
        case Judge =>
          S.judgeAssignment
        case Debater(i) =>
          S.debaterAssignment(i)
      }

    val bgStyle =
      resultOverUnderOpt match {
        case Some(overUnder) =>
          overUnder.bgStyle
        case None =>
          val myRoles  = roomMetadata.roleAssignments.filter(_._2 == userName).keySet
          val isMyTurn = myRoles.intersect(roomMetadata.currentSpeakers).nonEmpty
          if (isMyTurn) {
            ^.backgroundColor := Rgba(255, 255, 0, 0.25).toColorStyleString
          } else
            TagMod.empty
      }

    val turnSpan = {
      val speakers = roomMetadata.currentSpeakers
      val speakerElements = Helpers.delimitedTags[Vector, DebateRole](
        speakers.toVector,
        speaker => <.span(getRoleStyle(speaker))(speaker.toString)
      )
      <.span(S.bold)(speakerElements.toVdomArray, <.span(c"text-muted")("'s turn"))
        .when(speakerElements.nonEmpty)
    }

    val userRoleDisplay = {
      val myRoles = roomMetadata.roleAssignments.filter(_._2 == userName).keySet.toVector.sorted
      if (myRoles.isEmpty)
        None
      else
        Some {
          roomMetadata.result match {
            case Some(result) =>
              <.span(S.bold)(
                <.span(c"text-muted")("You were "),
                Helpers
                  .delimitedTags[Vector, DebateRole](
                    myRoles,
                    {
                      case Judge =>
                        <.span("Judge") // should just be black
                      case Debater(i) if i == result.correctAnswerIndex =>
                        <.span(S.correct, "Honest")
                      case Debater(_) =>
                        <.span(S.incorrect, "Dishonest")
                    }
                  )
                  .toVdomArray
              )
            case None =>
              <.span(S.bold)(
                <.span(c"text-muted")("You are "),
                Helpers
                  .delimitedTags[Vector, DebateRole](
                    myRoles,
                    {
                      case Judge =>
                        <.span(S.judgeAssignment, "Judge")
                      case d @ Debater(i) =>
                        <.span(S.debaterAssignment(i), d.toString)
                    }
                  )
                  .toVdomArray
              )
          }
        }
    }

    val boxTitle = ReactFragment(
      <.h5(c"card-title")(roomMetadata.name),
      <.h6(c"card-subtitle", c"mb-2")(
        resultOverUnderOpt match {
          case Some(overUnder) =>
            overUnder.label
          case None =>
            turnSpan
        }
      ),
      userRoleDisplay.map(<.h6(c"card-subtitle mb-2")(_))
    )

    val storyTitle = <.div("Story: ", <.i(roomMetadata.storyTitle))

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
      <.button(c"btn btn-danger")(
        "Delete",
        ^.onClick ==>
          ((e: ReactMouseEvent) => {
            e.stopPropagation();
            sendToMainChannel(DeleteRoom(isOfficial, roomMetadata.name))
          })
      )

    val timeDisplay = {

      val updateInstant = Instant.ofEpochMilli(roomMetadata.latestUpdateTime)
      val nowInstant    = Instant.ofEpochMilli(System.currentTimeMillis())

      val numDaysAgo = ChronoUnit.DAYS.between(updateInstant, nowInstant)

      val daysAgoStr =
        numDaysAgo match {
          case 0 =>
            "today"
          case 1 =>
            "yesterday"
          case i if i <= 7 =>
            s"$i days ago"
          case _ =>
            val zone = ZoneId.of("Z") // assumes UTC, because whatever
            val date = updateInstant.atZone(zone)
            val formatter =
              if (date.getYear() == nowInstant.atZone(zone).getYear()) {
                DateTimeFormatter.ofPattern("MMM d")
              } else
                DateTimeFormatter.ofPattern("MMM d YYYY")
            formatter.format(date)
        }

      val timeAgoStr =
        if (roomMetadata.result.isEmpty) {
          "Last speech " + daysAgoStr
        } else {
          daysAgoStr.capitalize
        }

      <.div(c"card-text text-muted")(<.small(timeAgoStr))
    }

    val selectableStyle =
      if (canEnterRoom)
        S.simpleSelectable
      else
        S.simpleUnselectable

    <.div(c"card", selectableStyle, bgStyle)(
      <.div(c"card-body")(
        boxTitle,
        <.div(c"card-text", c"mb-3".when(isAdmin))(
          storyTitle,
          roleAssignments,
          presentParticipants
        ),
        deleteRoom.when(isAdmin)
      ),
      <.div(S.timestampFooter)(timeDisplay),
      (^.onClick --> enterRoom(ConnectionSpec(isOfficial, roomMetadata.name, userName)))
        .when(canEnterRoom)
    )
  }
}
