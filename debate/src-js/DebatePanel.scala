package debate

import org.scalajs.jquery.jQuery

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._

import cats.implicits._

import jjm.ling.ESpan

import jjm.ui.Rgba
import jjm.ui.LocalState

object DebatePanel {
  // import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  // import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  val LocalSpans = new LocalState[Set[ESpan]]
  val LocalBool = new LocalState[Boolean]

  val curHighlightColor = Rgba(255, 255, 0, 0.8)
  val spanColorsByDebaterIndex = Map(
    0 -> Rgba(0, 0, 139, 0.4),
    1 -> Rgba(139, 0, 0, 0.4),
    2 -> Rgba(102, 51, 153, 0.4),
    3 -> Rgba(255, 140, 0, 0.4),
    4 -> Rgba(0, 206, 209, 0.4)
  )
  def getSpanColorForRole(role: Role) = role match {
    case Observer       => Rgba(0, 0, 0, 0.6)
    case Facilitator    => Rgba(285, 293, 200, 0.5)
    case Judge          => Rgba(0, 100, 0, 0.4)
    case Debater(index) => spanColorsByDebaterIndex(index)
  }

  /** Show whose turn it is. */
  def turnDisplay(
      roleOpt: Option[Role],
      currentTurns: Either[DebateResult, Map[Role, DebateTurnType]]
  ) = <.div(
    // NOTE: this assumes exactly one unique turn type will be present on the right side of the Either.
    // We expect this to be true but it isn't guaranteed in the types or anything.
    // I think this is the only part of the code that relies on that assumption;
    // in theory violating it could be useful at some point if the turntaking functionality becomes
    // more complex.
    currentTurns.map(_.values.head) match {
      case Left(result) =>
        <.span(
          s"The debate is over! The correct answer was ${answerLetter(result.correctAnswerIndex)}. ",
          s"The judge has earned a reward of ${result.judgeReward}."
        )
      case Right(turn) =>
        turn match {
          case DebateTurnType.SimultaneousSpeechesTurn(remainingDebaters, _, _) =>
            roleOpt match {
              case Some(Debater(index)) =>
                if (remainingDebaters.contains(index)) {
                  <.span(
                    "It is YOUR TURN. All debaters are constructing simultaneous speeches."
                  )
                } else {
                  <.span(
                    "Your speech has been received. Waiting for other debaters."
                  )
                }
              case _ =>
                <.span("All debaters are constructing simultaneous speeches.")
            }
          case DebateTurnType.DebaterSpeechTurn(index, _, _) =>
            roleOpt match {
              case Some(Debater(`index`)) =>
                <.span("It is YOUR TURN to make a speech.")
              case _ => <.span(s"Debaters are writing their speeches.")
            }
          case DebateTurnType.JudgeFeedbackTurn(_, _) =>
            roleOpt match {
              case Some(Judge) =>
                <.span("It is YOUR TURN as judge to give feedback.")
              case _ => <.span(s"It is the Judge's turn to give feedback.")
            }
        }
    }
  )

  /** Show the debate. */
  def apply(
      roomName: String,
      userId: Option[ParticipantId],
      debate: Debate,
      sendDebate: Debate => Callback
  ) = {
    import debate.{setup, rounds}
    val roleOpt = userId.map(_.role)
    val shouldShowSourceMaterial = roleOpt match {
      case Some(Facilitator | Debater(_)) => true
      case _                              => false
    }
    val inProgressSpeechStyle = roleOpt match {
      case None              => TagMod(S.noRoleOutline)
      case Some(Facilitator) => TagMod(S.facilitatorOutline)
      case Some(Observer)    => TagMod(S.observerOutline)
      case Some(Judge)       => TagMod(S.judgeOutline)
      case Some(Debater(index)) =>
        TagMod(S.answerOutline(index), S.debateWidthOffset(index))
    }

    // val isUsersTurn = role.exists(r =>
    //   currentTransitions.toOption
    //     .flatMap(_.giveSpeech.get(r))
    //     .nonEmpty
    // )
    // val charLimit = currentTransitions.fold(_ => -1, _.charLimit)
    // val quoteLimitOpt = currentTransitions.fold(_ => None, _.quoteLimit)

    val scrollDebateToBottom = Callback {
      val newSpeechesJQ = jQuery("#speeches")
      val newSpeechesDiv = newSpeechesJQ(0)
      newSpeechesJQ.scrollTop(
        newSpeechesDiv.scrollHeight - newSpeechesDiv.clientHeight
      )
    }
    // def maybeScrollDebateToBottom = {
    //   val speechesJQ = jQuery("#speeches")
    //   val speechesDiv = speechesJQ(0)
    //   val isScrolledToBottom =
    //     speechesDiv.scrollHeight - speechesJQ.scrollTop() - speechesJQ
    //       .outerHeight() < 1
    //   if (isScrolledToBottom) scrollDebateToBottom else Callback.empty
    // }


    // def getSpeechLength(speechSegments: Vector[SpeechSegment]) = {
    //   speechSegments.foldMap {
    //     case SpeechSegment.Text(text) => text.size
    //     case SpeechSegment.Quote(span) =>
    //       Helpers.renderSpan(setup.sourceMaterial.contents, span).size
    //   }
    // }
    // def getQuoteLength(speechSegments: Vector[SpeechSegment]) = {
    //   speechSegments.foldMap {
    //     case SpeechSegment.Text(_) => 0
    //     case SpeechSegment.Quote(span) =>
    //       Helpers.renderSpan(setup.sourceMaterial.contents, span).size
    //   }
    // }

    // val scrollDebateToBottom = Callback {
    //   val newSpeechesJQ = jQuery("#speeches")
    //   val newSpeechesDiv = newSpeechesJQ(0)
    //   newSpeechesJQ.scrollTop(
    //     newSpeechesDiv.scrollHeight - newSpeechesDiv.clientHeight
    //   )
    // }
    // def maybeScrollDebateToBottom = {
    //   val speechesJQ = jQuery("#speeches")
    //   val speechesDiv = speechesJQ(0)
    //   val isScrolledToBottom =
    //     speechesDiv.scrollHeight - speechesJQ.scrollTop() - speechesJQ
    //       .outerHeight() < 1
    //   if (isScrolledToBottom) scrollDebateToBottom else Callback.empty
    // }

    val debateSpansWithSpeaker = rounds.flatMap { round =>
      if (round.isComplete(setup.answers.size)) {
        round.allSpeeches.view
          .flatMap(speech => speech.allQuotes.map(speech.speaker -> _))
          .toVector
      } else {
        roleOpt
          .view
          .flatMap(role => round.allSpeeches.filter(_.speaker.role == role))
          .flatMap(speech => speech.allQuotes.map(speech.speaker -> _))
          .toVector
      }
    }

    val currentTransitions = debate.currentTransitions
    val userTurn = for {
      role <- roleOpt
      transitions <- currentTransitions.toOption
      turn <- transitions.giveSpeech.get(role)
    } yield turn
    val isUsersTurn = userTurn.nonEmpty

    LocalSpans.make(Set.empty[ESpan]) { curMessageSpans =>
      val highlights = debateSpansWithSpeaker.map { case (id, span) =>
        span -> getSpanColorForRole(id.role)
      } ++ curMessageSpans.value.toVector.map(_ -> curHighlightColor)

      <.div(S.debatePanel, S.spaceySubcontainer)(
        StoryPanel.Component(
          StoryPanel.Props(
            setup.sourceMaterial.contents,
            highlights,
            span => curMessageSpans.modState(_ + span)
          )
        ).when(shouldShowSourceMaterial),
        LocalQuotingMessage.make(
          curMessageSpans,
          s"debate-cookie-$roomName",
          didUpdate = _ => scrollDebateToBottom
        ) { currentMessage =>
          val currentMessageSpeechSegments =
            SpeechSegments.getFromString(currentMessage.value)

          <.div(S.debateSubpanel)(
            <.div(S.speechesSubpanel)(
              ^.id := "speeches",
              rounds.zipWithIndex.flatMap { case (round, roundIndex) =>
                Option(DebateRoundView.makeRoundHtml(
                  source = setup.sourceMaterial.contents,
                  roleOpt = roleOpt,
                  debateStartTime = debate.startTime,
                  numDebaters = setup.answers.size,
                  round,
                  roundIndex
                )).filter(_ =>
                  roleOpt.collect { case Facilitator | Debater(_) =>
                    ()
                  }.nonEmpty ||
                    round.isComplete(setup.answers.size)
                )
              }.toVdomArray,
              userId.whenDefined { userId =>
                DebateRoundView.makeSpeechHtml(
                  setup.sourceMaterial.contents,
                  DebateSpeech(userId, -1L, currentMessageSpeechSegments),
                  debate.startTime,
                  Some(userId.role),
                  inProgressSpeechStyle
                ).when(currentMessage.value.size > 0 && isUsersTurn)
              }
            ),
            turnDisplay(roleOpt, currentTransitions.map(_.currentTurns)),
            roleOpt.whenDefined { role =>
              currentTransitions.toOption.whenDefined { transitions =>
                <.div(S.col)(
                  <.div(S.col)(
                    transitions.giveSpeech.get(role).whenDefined { case turnDotPair =>
                      SpeechInput.speechInput(
                        debate,
                        sendDebate,
                        userId,
                        turnDotPair,
                        currentMessage)
                    },
                    transitions.undo.get(role).whenDefined {
                      case (speech, debateAfterUndo) =>
                        <.button(
                          "Undo",
                          ^.onClick --> (
                            sendDebate(debateAfterUndo) >>
                              currentMessage.setState(SpeechSegments.getString(speech))
                          )
                        )
                    }
                  )
                )
              }
            }
          )
        }
      )
    }
  }
}
