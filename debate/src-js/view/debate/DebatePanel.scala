package debate
package view.debate

import scala.collection.immutable.SortedSet

import cats.data.NonEmptySet
import cats.implicits._

import japgolly.scalajs.react.AsyncCallback
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.DotMap
import jjm.DotPair
import jjm.implicits._
import jjm.ling.ESpan
import jjm.ui.Rgba

import debate.MainChannelRequest
import debate.util.Local
import debate.view.lobby.TabNav
import debate.SpeechSegment.Quote
import debate.span2text

import Utils.ClassSetInterpolator

object DebatePanel {
  // import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  // import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  val curHighlightColor = Rgba(255, 255, 0, 0.8)
  val spanColorsByDebaterIndex = Map(
    0 -> Rgba(0, 0, 139, 0.4),
    1 -> Rgba(139, 0, 0, 0.4),
    2 -> Rgba(102, 51, 153, 0.4),
    3 -> Rgba(255, 140, 0, 0.4),
    4 -> Rgba(0, 206, 209, 0.4)
  )
  def getSpanColorForRole(role: Role) =
    role match {
      case Debater(index) =>
        spanColorsByDebaterIndex(index)
      case Judge =>
        Rgba(0, 100, 0, 0.4)
      case Facilitator =>
        Rgba(285, 293, 200, 0.5)
      case Observer | OfflineJudge | Peeper =>
        Rgba(0, 0, 0, 0.6)
    }

  /** Show whose turn it is. */
  def turnDisplay(
    roomName: String,
    assignments: Map[LiveDebateRole, String],
    userName: String,
    role: Role,
    currentTurns: Map[DebateRole, DebateTurnType],
    sendToMainChannel: MainChannelRequest => Callback
  ) = <.div {
    // NOTE: this assumes exactly one unique turn type will be present on the right side of the Either.
    // We expect this to be true but it isn't guaranteed in the types or anything.
    // I think this is the only part of the code that relies on that assumption;
    // in theory violating it could be useful at some point if the turntaking functionality becomes
    // more complex.

    val turn = currentTurns.values.head
    <.span(
      turn match {
        case DebateTurnType.SimultaneousSpeechesTurn(remainingDebaters, _, _) =>
          role match {
            case Debater(index) =>
              if (remainingDebaters.contains(index)) {
                <.span("It is YOUR TURN. All debaters are constructing simultaneous speeches.")
              } else {
                <.span("Your speech has been received. Waiting for other debaters.")
              }
            case _ =>
              <.span("All debaters are constructing simultaneous speeches.")
          }
        case DebateTurnType.DebaterSpeechTurn(index, _, _) =>
          role match {
            case Debater(`index`) =>
              <.span("It is YOUR TURN to make a speech.")
            case _ =>
              <.span(s"Debaters are writing their speeches.")
          }
        case DebateTurnType.JudgeFeedbackTurn(_, _, _) =>
          role match {
            case Judge =>
              <.span("It is YOUR TURN as judge to give feedback.")
            case _ =>
              <.span(s"It is the Judge's turn to give feedback.")
          }
        case DebateTurnType.NegotiateEndTurn(remainingDebaters) =>
          role match {
            case Debater(index) =>
              if (remainingDebaters.contains(index)) {
                <.span("It is YOUR TURN. Debaters are voting on whether to end the debate.")
              } else {
                <.span("Your vote has been received. Waiting for other debaters.")
              }
            case _ =>
              <.span("Debaters are voting on whether to end the debate.")
          }
        case DebateTurnType.OfflineJudgingTurn(offlineJudgments) =>
          role match {
            case OfflineJudge =>
              offlineJudgments.get(userName) match {
                case None =>
                  <.span("You are preparing to judge this debate offline.")
                case Some(OfflineJudgment(mode, _, _, None)) =>
                  <.span(s"You are judging this debate offline ($mode).")
                case Some(OfflineJudgment(mode, _, _, Some(_))) =>
                  <.span(f"The debate is over. You judged it offline ($mode).")
              }
            case _ =>
              <.span(s"The debate is over.")
          }
      },
      " ",
      NonEmptySet
        .fromSet(
          SortedSet(
            (
              currentTurns.keySet.flatMap(Role.liveDebateRole.getOption) --
                Role.liveDebateRole.getOption(role)
            ).flatMap(assignments.get).toSeq: _*
          )
        )
        .map { peopleWaitedOn =>
          Local[Boolean].make(false) { justPoked =>
            <.a(
              ^.href := "#",
              (
                ^.onClick --> {
                  sendToMainChannel(Poke(roomName, peopleWaitedOn)) >>
                    justPoked.setState(
                      true,
                      AsyncCallback.unit.delayMs(5000).completeWith(_ => justPoked.setState(false))
                    )
                }
              ).when(!justPoked.value),
              if (justPoked.value)
                "Poked."
              else
                "Click to poke them."
            )
          }
        }
    )
  }

  def visibleRounds(role: Role, debate: Debate) = debate
    .rounds
    .zip(
      debate
        .rounds
        .scanLeft(0) { case (n, round) =>
          round match {
            case JudgeFeedback(_, _, false) =>
              n + 1
            case _ =>
              n
          }
        }
    )
    .filter { case (round, _) =>
      role.canSeeIntermediateArguments || round.isComplete(debate.setup.numDebaters)
    }

  def debateSpansWithSpeaker(role: Role, numDebaters: Int, rounds: Vector[DebateRound]) = rounds
    .flatMap { round =>
      if (round.isComplete(numDebaters)) {
        round
          .allSpeeches
          .view
          .flatMap { case (role, speech) =>
            speech.allQuotes.map(role -> _)
          }
          .toVector
      } else {
        round.allSpeeches.get(role).toVector.flatMap(_.allQuotes.map(role -> _))
      }
    }

  def getHighlights(
    role: Role,
    numDebaters: Int,
    rounds: Vector[DebateRound],
    curMessageSpans: Set[ESpan]
  ) =
    debateSpansWithSpeaker(role, numDebaters, rounds).map { case (role, span) =>
      span -> getSpanColorForRole(role)
    } ++ curMessageSpans.toVector.map(_ -> curHighlightColor)

  def getInProgressSpeechStyle(role: Role) =
    role match {
      case Debater(index) =>
        TagMod(S.answerOutline(index), S.debateWidthOffset(index))
      case Judge =>
        TagMod(S.judgeOutline)
      case Facilitator =>
        TagMod(S.facilitatorOutline)
      case Observer | OfflineJudge | Peeper =>
        TagMod(S.observerOutline)
    }

  /** Show the debate. */
  def apply(
    profiles: Set[String],
    roomName: String,
    userName: String,
    role: Role,
    debate: StateSnapshot[Debate],
    anonymize: Boolean,
    sendToMainChannel: MainChannelRequest => Callback
  ) = {
    import debate.value.{setup, rounds}

    def scratchpad(debateRole: DebateRole) = debate.zoomStateL(
      Debate
        .scratchpads
        .composeLens(Optics.at(debateRole))
        .composeIso(optionIsoWithEmpty[Vector[Vector[SpeechSegment]]])
    )

    val currentTransitions = debate.value.currentTransitions
    val userTurn = Role.debateRole.getOption(role).flatMap(currentTransitions.giveSpeech.get)
    // for {
    //   debateRole  <- role.asLiveDebateRoleOpt
    //   transitions <- currentTransitions.toOption
    //   turn        <- transitions.giveSpeech.get(debateRole)
    // } yield turn
    val isUsersTurn = userTurn.nonEmpty

    val timeForFeedback =
      debate.value.isOver &&
        (setup.roles.values.toVector.contains(userName) ||
          debate.value.offlineJudgingResults.get(userName).exists(_.result.nonEmpty))

    val canSeeDebate =
      !(role == OfflineJudge && debate.value.offlineJudgingResults.get(userName).isEmpty)

    val nonDebateRoles = Set(Observer, Facilitator)
    val canSeeResult   = debate.value.isOver && (timeForFeedback || nonDebateRoles.contains(role))
    val getRewardForJudgment =
      (turnNum: Int, judgment: Vector[Double]) =>
        Option(
          debate
            .value
            .setup
            .rules
            .scoringFunction
            .eval(turnNum, judgment, debate.value.setup.correctAnswerIndex)
        ).filter(_ => canSeeResult)

    Local[String].syncedWithLocalStorage(s"debate-message-$roomName", "") { currentMessage =>
      val currentMessageSpeechSegments = SpeechSegments.getFromString(currentMessage.value)
      val curMessageSpans =
        currentMessageSpeechSegments
          .collect { case Quote(span) =>
            span
          }
          .toSet
      val uploadedResponse = debate.value.feedback.get(userName)
      val uploadedAnswers  = uploadedResponse.map(_.answers)
      val workingAnswers: DotMap[Option, Feedback.Key] = uploadedAnswers
        .map { answers =>
          DotMap(
            answers.iterator.toList.map(pair => DotPair[Option](pair.fst)(Option(pair.snd))): _*
          )
        }
        .getOrElse(Feedback.initAnswers(setup, role))
      Local[DotMap[Option, Feedback.Key]].make(workingAnswers) { surveyAnswers =>
        val leftPanelTabs =
          Vector(
            Option(
              "Story" ->
                TabNav.tab(
                  StoryPanel(
                    setup.sourceMaterial.contents,
                    getHighlights(role, setup.numDebaters, rounds, curMessageSpans),
                    span => currentMessage.modState(_ + span2text(span))
                  )
                )
            ).filter(_ => role.canSeeStory),
            Option(
              "Quotes" ->
                TabNav.tab(
                  ConsolidatedQuotesPanel(
                    setup.sourceMaterial.contents,
                    getHighlights(role, setup.numDebaters, rounds, curMessageSpans)
                  )
                )
            ),
            role
              .asDebateRoleOpt
              .map(debateRole =>
                "Scratchpad" ->
                  TabNav.tabWithNotifications(
                    debate.value.scratchpads.get(debateRole).foldMap(_.size),
                    mod = c"badge-secondary"
                  )(
                    <.div(c"card-body") {
                      ScratchpadPanel(
                        setup.sourceMaterial.contents,
                        currentMessage,
                        scratchpad(debateRole)
                      )
                    }
                  )
              ),
            Option(
              "Feedback Survey" ->
                TabNav.tabWithBadge("!")(
                  FeedbackSurvey(
                    setup,
                    profiles,
                    role,
                    uploadedResponse,
                    surveyAnswers,
                    submit =
                      response =>
                        debate
                          .zoomStateL(Debate.feedback.composeLens(Optics.at(userName)))
                          .setState(Some(response))
                  )
                )
            ).filter(_ => timeForFeedback)
          ).flatten

        <.div(S.debatePanel, S.spaceySubcontainer)(
          if (leftPanelTabs.nonEmpty) {
            Option(<.div(S.debateSubpanel)(TabNav(s"$roomName-story/feedback")(leftPanelTabs: _*)))
          } else
            None,
          <.div(S.debateSubpanel)(
            <.div(S.speechesSubpanel)(
                ^.id := "speeches",
                visibleRounds(role, debate.value)
                  .zipWithIndex
                  .flatMap { case ((round, numPreviousContinues), roundIndex) =>
                    Option(
                      DebateRoundView.makeRoundHtml(
                        source = setup.sourceMaterial.contents,
                        userName = userName,
                        role = role,
                        anonymize = anonymize,
                        debateStartTime = debate.value.startTime,
                        numDebaters = setup.answers.size,
                        numPreviousContinues = numPreviousContinues,
                        getRewardForJudgment = getRewardForJudgment,
                        round,
                        roundOpt =>
                          debate
                            .zoomStateL(Debate.rounds)
                            .modState(rounds =>
                              roundOpt match {
                                case None =>
                                  rounds.remove(roundIndex)
                                case Some(r) =>
                                  rounds.updated(roundIndex, r)
                              }
                            )
                      )(^.key := s"round-$roundIndex")
                    )
                  }
                  .toVdomArray,
                DebateRoundView
                  .makeSpeechHtml(
                    if (role.canSeeStory)
                      setup.sourceMaterial.contents
                    else
                      Vector(),
                    role,
                    DebateSpeech(userName, -1L, currentMessageSpeechSegments),
                    debate.value.startTime,
                    role,
                    userName,
                    anonymize,
                    getInProgressSpeechStyle(role)
                  )
                  .when(currentMessage.value.size > 0 && isUsersTurn)
              )
              .when(canSeeDebate),
            <.span(
                s"The correct answer was ${answerLetter(debate.value.setup.correctAnswerIndex)}. "
              )
              .when(canSeeResult),
            turnDisplay(
              roomName,
              debate.value.setup.roles,
              userName,
              role,
              currentTransitions.currentTurns,
              sendToMainChannel
            ),
            <.div(S.col)(
              <.div(S.col)(
                userTurn.whenDefined { case turnDotPair =>
                  SpeechInput.speechInput(
                    debate,
                    userName,
                    role,
                    turnDotPair,
                    currentMessage,
                    saveCallbackOpt = role
                      .asDebateRoleOpt
                      .map(debateRole =>
                        scratchpad(debateRole).modState(_ :+ currentMessageSpeechSegments) >>
                          currentMessage.setState("")
                      )
                  )
                },
                role
                  .asDebateRoleOpt
                  .flatMap(currentTransitions.undo.get)
                  .whenDefined { case (speech, debateAfterUndo) =>
                    <.button(
                      "Undo",
                      ^.onClick -->
                        (debate.setState(debateAfterUndo) >>
                          currentMessage.setState(SpeechSegments.getString(speech)))
                    )
                  }
              )
            )
          )
        )
      }
    }
  }
}
