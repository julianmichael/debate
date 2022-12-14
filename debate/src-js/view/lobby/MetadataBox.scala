package debate
package view.lobby

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
  import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def getRoleStyle(role: DebateRole): TagMod =
    role match {
      case Judge =>
        S.judgeAssignment
      case Debater(i) =>
        S.debaterAssignment(i)
    }

  def getRoleStyle(role: LeaderboardCategory): TagMod =
    role match {
      case LeaderboardCategory.Judge =>
        TagMod.empty
      case LeaderboardCategory.HonestDebater =>
        S.correct
      case LeaderboardCategory.DishonestDebater =>
        S.incorrect
    }

  def getLeaderboardCategoryForRole(role: DebateRole, correctAnswerIndex: Int) =
    role match {
      case Judge =>
        LeaderboardCategory.Judge
      case Debater(i) if i == correctAnswerIndex =>
        LeaderboardCategory.HonestDebater
      case Debater(_) =>
        LeaderboardCategory.DishonestDebater
    }

  def getDebateEndReasonStyle(endReason: DebateEndReason): TagMod =
    endReason match {
      case DebateEndReason.JudgeDecided =>
        TagMod.empty
      case DebateEndReason.TimeUp =>
        c"text-muted"
      case DebateEndReason.MutualAgreement =>
        c"text-muted"
    }

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

    case class ResultDescription(label: VdomNode, bgStyle: TagMod)

    val resultDescriptionOpt = roomMetadata
      .result
      // .flatMap(_.judgingInfo)
      .map { result =>
        val endedBy = <.span(
          <.span(c"text-muted")("Ended by "),
          <.span(getDebateEndReasonStyle(result.endedBy))(
            result.endedBy match {
              case DebateEndReason.JudgeDecided =>
                "Judge"
              case DebateEndReason.TimeUp =>
                "Time"
              case DebateEndReason.MutualAgreement =>
                "Agreement"
            }
          )
        )

        val overUnderOpt = result
          .judgingInfo
          .map { info =>
            val correctConfidence = info.finalJudgement(info.correctAnswerIndex) * 100
            val otherConfidences = info
              .finalJudgement
              .remove(info.correctAnswerIndex)
              .sortBy(-_)
              .map(_ * 100)

            val label = <.span(
              <.span(S.correct)(f"$correctConfidence%.0f%%"),
              "/",
              Utils
                .delimitedTags[Vector, Double](
                  otherConfidences,
                  getTag = conf => <.span(S.incorrect)(f"$conf%.0f%%"),
                  delimiter = "/"
                )
                .toVdomArray
            )

            label
          }

        val style =
          result.judgingInfo match {
            case None =>
              TagMod(^.backgroundColor := "#eee")
            case Some(info) =>
              val correctConfidence = info.finalJudgement(info.correctAnswerIndex) * 100
              val otherConfidences = info
                .finalJudgement
                .remove(info.correctAnswerIndex)
                .sortBy(-_)
                .map(_ * 100)

              otherConfidences.headOption match {
                case None =>
                  TagMod.empty
                case Some(secondGuessConfidence) =>
                  val correctnessScore =
                    correctConfidence / (correctConfidence + secondGuessConfidence)
                  val opacity = math.abs(correctnessScore - 0.5)
                  val color =
                    if (correctnessScore > 0.5)
                      Rgba(0, 128, 0, opacity) // green
                    else
                      Rgba(220, 20, 60, opacity) // crimson
                  TagMod(^.backgroundColor := color.toColorStyleString)
              }
          }

        ResultDescription(
          overUnderOpt match {
            case Some(pctsLabel) =>
              <.span(pctsLabel, <.span(c"text-muted")(" ("), endedBy, <.span(c"text-muted")(")"))
            case None =>
              endedBy
          },
          style
        )
      }

    val bgStyle =
      resultDescriptionOpt match {
        case Some(description) =>
          description.bgStyle
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
      val speakerElements = Utils.delimitedTags[Vector, DebateRole](
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
                Utils
                  .delimitedTags[Vector, DebateRole](
                    myRoles,
                    { role =>
                      val category = getLeaderboardCategoryForRole(role, result.correctAnswerIndex)
                      val style    = getRoleStyle(category)
                      <.span(style, category.shortString)
                    }
                  )
                  .toVdomArray
              )
            case None =>
              <.span(S.bold)(
                <.span(c"text-muted")("You are "),
                Utils
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
        resultDescriptionOpt match {
          case Some(description) =>
            description.label
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
              Utils
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
        Utils.delimitedSpans(roomMetadata.currentParticipants.toList.sorted).toVdomArray
      )
      .when(roomMetadata.currentParticipants.nonEmpty)

    val deleteRoom =
      <.button(c"btn btn-sm btn-outline-danger")(
        <.i(c"bi bi-x"),
        " Delete",
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
