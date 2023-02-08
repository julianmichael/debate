package debate
package view.debate

import cats.implicits._

import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import jjm.ling.ESpan
import jjm.ui.Rgba

import debate.util.Local

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
      case Observer | TimedOfflineJudge =>
        Rgba(0, 0, 0, 0.6)
    }

  /** Show whose turn it is. */
  def turnDisplay(
    roleOpt: Option[DebateRole],
    currentTurns: Either[DebateResult, Map[DebateRole, DebateTurnType]]
  ) = <.div(
    // NOTE: this assumes exactly one unique turn type will be present on the right side of the Either.
    // We expect this to be true but it isn't guaranteed in the types or anything.
    // I think this is the only part of the code that relies on that assumption;
    // in theory violating it could be useful at some point if the turntaking functionality becomes
    // more complex.
    currentTurns.map(_.values.head) match {
      case Left(result) =>
        <.span(
          s"The debate is over! ",
          s"The correct answer was ${answerLetter(result.correctAnswerIndex)}. ",
          result
            .judgingInfo
            .map(judgingResult =>
              <.span(s"The judge has earned a reward of ${judgingResult.judgeReward}.")
            )
        )
      case Right(turn) =>
        turn match {
          case DebateTurnType.SimultaneousSpeechesTurn(remainingDebaters, _, _) =>
            roleOpt match {
              case Some(Debater(index)) =>
                if (remainingDebaters.contains(index)) {
                  <.span("It is YOUR TURN. All debaters are constructing simultaneous speeches.")
                } else {
                  <.span("Your speech has been received. Waiting for other debaters.")
                }
              case _ =>
                <.span("All debaters are constructing simultaneous speeches.")
            }
          case DebateTurnType.DebaterSpeechTurn(index, _, _) =>
            roleOpt match {
              case Some(Debater(`index`)) =>
                <.span("It is YOUR TURN to make a speech.")
              case _ =>
                <.span(s"Debaters are writing their speeches.")
            }
          case DebateTurnType.JudgeFeedbackTurn(_, _, _) =>
            roleOpt match {
              case Some(Judge) =>
                <.span("It is YOUR TURN as judge to give feedback.")
              case _ =>
                <.span(s"It is the Judge's turn to give feedback.")
            }
          case DebateTurnType.NegotiateEndTurn(remainingDebaters) =>
            roleOpt match {
              case Some(Debater(index)) =>
                if (remainingDebaters.contains(index)) {
                  <.span("It is YOUR TURN. Debaters are voting on whether to end the debate.")
                } else {
                  <.span("Your vote has been received. Waiting for other debaters.")
                }
              case _ =>
                <.span("Debaters are voting on whether to end the debate.")
            }
        }
    }
  )

  def visibleRounds(role: Role, debate: Debate) = debate
    .rounds
    .filter { round =>
      val isFacilitatorOrDebater =
        role match {
          case Facilitator | Debater(_) =>
            true
          case _ =>
            false
        }
      isFacilitatorOrDebater || round.isComplete(debate.setup.numDebaters)
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
      case Observer | TimedOfflineJudge =>
        TagMod(S.observerOutline)
    }

  /** Show the debate. */
  def apply(roomName: String, userName: String, role: Role, debate: StateSnapshot[Debate]) = {
    import debate.value.{setup, rounds}

    val currentTransitions = debate.value.currentTransitions
    val userTurn =
      for {
        debateRole  <- role.asDebateRoleOpt
        transitions <- currentTransitions.toOption
        turn        <- transitions.giveSpeech.get(debateRole)
      } yield turn
    val isUsersTurn = userTurn.nonEmpty

    Local[Set[ESpan]].make(Set.empty[ESpan]) { curMessageSpans =>
      <.div(S.debatePanel, S.spaceySubcontainer)(
        StoryPanel(
          setup.sourceMaterial.contents,
          getHighlights(role, setup.numDebaters, rounds, curMessageSpans.value),
          span => curMessageSpans.modState(_ + span)
        ).when(
          role match {
            case Facilitator | Debater(_) =>
              true
            case _ =>
              false
          }
        ),
        LocalQuotingMessage.make(curMessageSpans, s"debate-message-$roomName") { currentMessage =>
          val currentMessageSpeechSegments = SpeechSegments.getFromString(currentMessage.value)

          <.div(S.debateSubpanel)(
            <.div(S.speechesSubpanel)(
              ^.id := "speeches",
              visibleRounds(role, debate.value)
                .zipWithIndex
                .flatMap { case (round, roundIndex) =>
                  Option(
                    DebateRoundView.makeRoundHtml(
                      source = setup.sourceMaterial.contents,
                      role = role,
                      debateStartTime = debate.value.startTime,
                      numDebaters = setup.answers.size,
                      round
                    )(^.key := s"round-$roundIndex")
                  )
                }
                .toVdomArray,
              DebateRoundView
                .makeSpeechHtml(
                  setup.sourceMaterial.contents,
                  role,
                  DebateSpeech(userName, -1L, currentMessageSpeechSegments),
                  debate.value.startTime,
                  role,
                  getInProgressSpeechStyle(role)
                )
                .when(currentMessage.value.size > 0 && isUsersTurn)
            ),
            turnDisplay(role.asDebateRoleOpt, currentTransitions.map(_.currentTurns)),
            currentTransitions
              .toOption
              .whenDefined { transitions =>
                <.div(S.col)(
                  <.div(S.col)(
                    role
                      .asDebateRoleOpt
                      .flatMap(transitions.giveSpeech.get)
                      .whenDefined { case turnDotPair =>
                        SpeechInput.speechInput(debate, userName, role, turnDotPair, currentMessage)
                      },
                    role
                      .asDebateRoleOpt
                      .flatMap(transitions.undo.get)
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
              }
          )
        }
      )
    }
  }
}
