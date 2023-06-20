package debate
package view.lobby

import java.time.Instant
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import jjm.implicits._
import jjm.ui.Rgba

object MetadataBox {
  import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def getRoleStyle(role: LiveDebateRole): TagMod =
    role match {
      case Judge =>
        S.judgeAssignment
      case Debater(i) =>
        S.debaterAssignment(i)
    }

  def getRoleStyle(role: LeaderboardCategory): TagMod =
    role match {
      case LeaderboardCategory.OfflineJudge =>
        TagMod.empty
      case LeaderboardCategory.Judge =>
        TagMod.empty
      case LeaderboardCategory.HonestDebater =>
        S.correct
      case LeaderboardCategory.DishonestDebater =>
        S.incorrect
    }

  def getLeaderboardCategoryForRole(role: DebateRole, correctAnswerIndex: Int) =
    role match {
      case OfflineJudge =>
        LeaderboardCategory.OfflineJudge
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

  def getColorFromCorrectnessScore(correctnessScore: Double) = {
    val opacity = math.abs(correctnessScore - 0.5)
    val color =
      if (correctnessScore > 0.5)
        Rgba(0, 128, 0, opacity) // green
      else
        Rgba(220, 20, 60, opacity) // crimson
    color
  }

  def apply(
    storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]],
    presentDebaters: Set[String],
    roomMetadata: RoomMetadata,
    isOfficial: Boolean,
    userName: String,
    isAdmin: Boolean,
    hideResults: StateSnapshot[Boolean],
    anonymize: StateSnapshot[Boolean],
    enterRoom: ConnectionSpec => CallbackTo[Unit],
    sendToMainChannel: debate.MainChannelRequest => japgolly.scalajs.react.CallbackTo[Unit]
  ) = {
    val stats = storyRecord.get(userName).flatMap(_.get(roomMetadata.sourceMaterialId)).combineAll

    val debatesUserMustJudgeFirst = stats.debatesUserMustJudgeFirst(roomMetadata.name)

    val mustWaitForDebateToEnd =
      roomMetadata.result.isEmpty &&
        !roomMetadata.roleAssignments.values.toSet.contains(userName) &&
        (roomMetadata.offlineJudgeAssignments.contains(userName) ||
          (!stats.hasReadStory && stats.canJudgeMore && userName != adminUsername))

    val canEnterRoom =
      userName.nonEmpty && debatesUserMustJudgeFirst.isEmpty && !mustWaitForDebateToEnd

    val assignedLiveParticipants   = roomMetadata.roleAssignments.values.toSet
    val participantsPresentInLobby = assignedLiveParticipants.intersect(presentDebaters)
    val lobbyPresenceIndicator = Option(
      <.div(
        S.lobbyPresenceIndicator,
        if (participantsPresentInLobby.size == assignedLiveParticipants.size)
          c"text-primary"
        else
          c"text-muted"
      )(
        s"${participantsPresentInLobby.size}/${assignedLiveParticipants.size} online",
        (0 until assignedLiveParticipants.size).toVdomArray { i =>
          if (i >= participantsPresentInLobby.size) {
            <.i(S.lobbyPresenceCircle, c"bi bi-circle")
          } else
            <.i(S.lobbyPresenceCircle, c"bi bi-circle-fill")
        }
      )
    ).filter(_ =>
      assignedLiveParticipants.size > 0 &&
        (roomMetadata.status match {
          case RoomStatus.WaitingToBegin | RoomStatus.InProgress =>
            true
          case _ =>
            false
        })
    )

    case class ResultDescription(
      label: VdomNode,
      bgStyle: TagMod,
      offlineResults: Option[VdomNode],
      feedbackNotice: Option[VdomNode]
    )

    val resultDescriptionOpt = RoomStatus
      .complete
      .getOption(roomMetadata.status)
      .filter(_ => !hideResults.value)
      // .flatMap(_.judgingInfo)
      .map { case RoomStatus.Complete(result, offlineJudgingResults, feedbackProviders) =>
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

        def getBgColorModFromJudgment(judgment: Vector[Double]) = {
          val correctConfidence = judgment(result.correctAnswerIndex) * 100
          val otherConfidences  = judgment.remove(result.correctAnswerIndex).sortBy(-_).map(_ * 100)

          otherConfidences.headOption match {
            case None =>
              TagMod.empty
            case Some(secondGuessConfidence) =>
              val correctnessScore = correctConfidence / (correctConfidence + secondGuessConfidence)
              TagMod(
                ^.backgroundColor :=
                  getColorFromCorrectnessScore(correctnessScore).toColorStyleString
              )
          }
        }

        val style = {
          val avgJudgment =
            (
              offlineJudgingResults.flatMap(_._2.result).map(_.distribution) ++
                result.judgingInfo.map(_.finalJudgement)
            ).view.transpose.map(v => v.sum / v.size).toVector
          getBgColorModFromJudgment(avgJudgment)
        }

        val offlineResults = Option {
          val judgmentElements = offlineJudgingResults
            .toVector
            .flatMap(p => p._2.result.map(p._1 -> _))
            .sortBy(_._2.feedback.timestamp)
            .map { case (judge, offlineResult) =>
              judge ->
                <.div(
                  ^.key := s"offline-judgment-$judge",
                  Utils.probabilityBar(
                    TagMod.empty,
                    Vector(
                      Utils.ProbabilityBarItem(
                        offlineResult.distribution(result.correctAnswerIndex),
                        S.correctBg
                      ),
                      Utils.ProbabilityBarItem(
                        1.0 - offlineResult.distribution(result.correctAnswerIndex),
                        S.incorrectBg
                      )
                    )
                  )
                )
            }

          val judgmentsDisplay =
            if (anonymize.value) {
              judgmentElements.map(_._2).toVdomArray
            } else
              judgmentElements.toVdomArray { case (judge, bar) =>
                <.div(S.row, ^.key := s"offline-judge-$judge")(
                  <.div(S.judgmentBarLabel)(judge.takeWhile(_ != ' ')),
                  <.div(c"w-100")(bar)
                )
              }

          <.div(<.div(c"small text-center mt-1")("Offline Judgments"), judgmentsDisplay)
        }.filter(_ => offlineJudgingResults.nonEmpty)

        val peopleMissingFeedback =
          (roomMetadata.roleAssignments.values.toSet ++ offlineJudgingResults.keySet) --
            feedbackProviders
        val feedbackNotice = Option(
          <.div(
            <.span(S.awaitingFeedbackStatusLabel)("Awaiting feedback from: "),
            Utils.delimitedSpans(peopleMissingFeedback.toList.sorted).toVdomArray
          )
        ).filter(_ => !anonymize.value && peopleMissingFeedback.nonEmpty)

        ResultDescription(
          overUnderOpt match {
            case Some(pctsLabel) =>
              <.span(pctsLabel, <.span(c"text-muted")(" ("), endedBy, <.span(c"text-muted")(")"))
            case None =>
              endedBy
          },
          style,
          offlineResults,
          feedbackNotice
        )
      }

    val bgStyle =
      resultDescriptionOpt match {
        case Some(description) =>
          description.bgStyle
        case None =>
          val myRoles  = roomMetadata.roleAssignments.filter(_._2 == userName).keySet
          val isMyTurn = myRoles.intersect(roomMetadata.currentSpeakers).nonEmpty
          val isAssignedAndReadyToJudge =
            roomMetadata.offlineJudgeAssignments.contains(userName) &&
              RoomStatus
                .complete
                .getOption(roomMetadata.status)
                .flatMap(_.offlineJudgingResults.get(userName))
                .forall(_.result.isEmpty)
          if (isMyTurn || isAssignedAndReadyToJudge) {
            TagMod(S.attentionBackground)
          } else
            TagMod.empty
      }

    val myRoles =
      (
        roomMetadata.roleAssignments.filter(_._2 == userName).keySet ++
          RoomStatus
            .complete
            .getOption(roomMetadata.status)
            .filter(_.offlineJudgingResults.get(userName).exists(_.result.nonEmpty))
            .as(OfflineJudge)
      ).toVector.sorted

    val turnSpan = {
      val speakers = roomMetadata.currentSpeakers
      val (speakerElements, lastIsPlural) =
        if (
          myRoles.existsAs { case Debater(_) =>
            true
          }
        )
          Utils.delimitedTags[Vector, LiveDebateRole](
            speakers.toVector,
            speaker => <.span(getRoleStyle(speaker))(speaker.toString)
          ) -> false
        else { // you don't have privileges to see which debater's turn it is
          val roleSets =
            speakers
              .map {
                case Debater(_) =>
                  "Debaters"
                case Judge =>
                  "Judge"
              }
              .toVector
              .sorted
          def getStyle(roleSet: String) =
            roleSet match {
              case "Debaters" =>
                S.bold: TagMod
              case "Judge" =>
                getRoleStyle(Judge)
            }
          Utils.delimitedTags[Vector, String](
            roleSets,
            roleSet => <.span(getStyle(roleSet))(roleSet)
          ) -> (roleSets.lastOption == Some("Debaters"))
        }
      val optionalS =
        if (lastIsPlural)
          ""
        else
          "s"
      <.span(S.bold)(speakerElements.toVdomArray, <.span(c"text-muted")(s"'$optionalS turn"))
        .when(speakerElements.nonEmpty)
    }

    val userRoleDisplay =
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
                      case OfflineJudge =>
                        <.span("an ", <.span(S.judgeAssignment, "Offline Judge"))
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

    val boxTitle = ReactFragment(
      <.h5(c"card-title")(roomMetadata.name),
      Option(
        <.h6(c"card-subtitle", c"mb-2")(
          resultDescriptionOpt match {
            case Some(description) =>
              description.label
            case None =>
              turnSpan
          }
        )
      ).filter(_ => canEnterRoom),
      userRoleDisplay.map(<.h6(c"card-subtitle mb-2")(_))
    )

    val storyTitle = {
      import DebateProgressLabel.{Assigned, Begun}
      val numDebated        = (stats.debating - Assigned - Begun).unorderedFoldMap(_.size)
      val numDebating       = stats.debating.get(Begun).foldMap(_.size)
      val numDebatedPending = stats.debating.get(Assigned).foldMap(_.size)
      val numJudged         = (stats.allJudging - Assigned - Begun).unorderedFoldMap(_.size)
      val numJudging        = stats.allJudging.get(Begun).foldMap(_.size)
      val numJudgedPending  = stats.allJudging.get(Assigned).foldMap(_.size)
      def nTimes(n: Int) =
        if (n == 1)
          "1 time"
        else
          s"$n times"
      <.div(
        <.div("Story: ", <.i(roomMetadata.storyTitle)),
        <.div(c"small text-muted")(
            <.span(
              s"Debated ",
              <.span(^.color.black)(nTimes(numDebated)),
              <.span(s", $numDebating in progress").when(numDebating > 0),
              <.span(s", $numDebatedPending pending").when(numDebatedPending > 0)
            )
          )
          .when(numDebated + numDebatedPending > 0),
        <.div(c"small text-muted")(
            <.span(
              s"Judged ",
              <.span(^.color.black)(nTimes(numJudged)),
              <.span(s", $numJudging in progress").when(numJudging > 0),
              <.span(s", $numJudgedPending pending").when(numJudgedPending > 0)
            )
          )
          .when(numJudged + numJudgedPending > 0),
        <.div(c"small", ^.color.red)(
            "Must first judge: ",
            Utils
              .delimitedTags[Vector, String](
                debatesUserMustJudgeFirst.toVector.sorted,
                // getTag = { case (room, label) =>
                getTag =
                  name =>
                    <.a(
                      ^.href := "#",
                      name,
                      ^.onClick --> enterRoom(ConnectionSpec(isOfficial, name, userName))
                    )
              )
              .toVdomArray
          )
          .when(debatesUserMustJudgeFirst.nonEmpty),
        <.div(c"small", ^.color.red)("Debate must end before you can judge.")
          .when(mustWaitForDebateToEnd)
      )
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
          ),
        Option(roomMetadata.offlineJudgeAssignments)
          .filter(_.nonEmpty)
          .map(roles =>
            <.div(
              "Offline judges: ",
              Utils
                .delimitedTags[Vector, (String, Option[OfflineJudgingMode])](
                  roles.toVector,
                  getTag = { case (name, _) =>
                    <.span(
                      // S.debaterAssignment(role.answerIndex),
                      ^.fontWeight.bold.when(roomMetadata.currentParticipants.contains(name))
                    )(name)
                  }
                )
                .toVdomArray
            )
          )
      )
    }

    // val presentParticipants = {

    //   val participants =
    //     if (anonymize.value) {
    //       roomMetadata
    //         .currentParticipants
    //         .view
    //         .flatMap(name => roomMetadata.roleAssignments.find(_._2 == name).map(_._1))
    //         .map(_.toString)
    //         .toList
    //         .sorted
    //     } else
    //       roomMetadata.currentParticipants.toList.sorted
    //   <.div("In debate: ", Utils.delimitedSpans(participants).toVdomArray).when(participants.nonEmpty)
    // }

    val deleteRoomButton =
      <.button(c"btn btn-sm btn-outline-danger mt-1")(
        <.i(c"bi bi-x"),
        " Delete",
        ^.onClick ==>
          ((e: ReactMouseEvent) => {
            e.stopPropagation();
            sendToMainChannel(DeleteRoom(isOfficial, roomMetadata.name))
          })
      )

    def inspectButton(text: String, toggle: StateSnapshot[Boolean]) = {
      val icon =
        if (toggle.value)
          "bi-eye-slash"
        else
          "bi-eye"

      <.button(c"btn btn-sm btn-outline-primary mr-1 mt-1")(
        <.i(c"bi $icon"),
        s" $text",
        ^.onClick ==>
          ((e: ReactMouseEvent) => {
            e.stopPropagation();
            toggle.modState(!_)
          })
      )
    }

    val inspectPeopleButton  = inspectButton("People", anonymize)
    val inspectResultsButton = inspectButton("Results", hideResults)

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
        if (roomMetadata.result.nonEmpty) {
          daysAgoStr.capitalize
        } else if (roomMetadata.status == RoomStatus.WaitingToBegin) {
          "Created " + daysAgoStr
        } else {
          "Last speech " + daysAgoStr
        }

      <.div(c"card-text text-muted")(<.small(timeAgoStr))
    }

    val selectableStyle =
      if (canEnterRoom)
        S.simpleSelectable
      else
        S.simpleUnselectable

    <.div(c"card", selectableStyle, bgStyle)(
      lobbyPresenceIndicator,
      <.div(c"card-body")(
        boxTitle,
        <.div(c"card-text", c"mb-2".when(isAdmin))(
          storyTitle,
          roleAssignments.when(!anonymize.value),
          // presentParticipants,
          resultDescriptionOpt.map(_.offlineResults),
          resultDescriptionOpt.map(_.feedbackNotice)
        ),
        inspectPeopleButton.when(isAdmin),
        inspectResultsButton.when(isAdmin),
        deleteRoomButton.when(isAdmin)
      ),
      <.div(S.timestampFooter)(timeDisplay),
      (^.onClick --> enterRoom(ConnectionSpec(isOfficial, roomMetadata.name, userName)))
        .when(canEnterRoom)
    )
  }
}
