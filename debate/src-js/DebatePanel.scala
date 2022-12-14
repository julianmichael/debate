package debate

import org.scalajs.jquery.jQuery

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._

import cats.implicits._

import org.scalajs.dom.ext.KeyCode

import jjm.ling.ESpan

import jjm.ui.Rgba
import jjm.ui.LocalState

import java.time.{Instant, ZoneId}

class DebatePanel(
    S: Styles.type,
    V: jjm.ui.View
) {

  val LocalSpans = new LocalState[Set[ESpan]]
  val LocalProbs = new LocalState[Vector[Double]]
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
      turnOrResult: Either[DebateResult, DebateTurnType]
  ) = <.div(
    turnOrResult match {
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
    val role = userId.map(_.role)
    val shouldShowSourceMaterial = role match {
      case Some(Facilitator | Debater(_)) => true
      case _                              => false
    }
    val inProgressSpeechStyle = role match {
      case None              => TagMod(S.noRoleOutline)
      case Some(Facilitator) => TagMod(S.facilitatorOutline)
      case Some(Observer)    => TagMod(S.observerOutline)
      case Some(Judge)       => TagMod(S.judgeOutline)
      case Some(Debater(index)) =>
        TagMod(S.answerOutline(index), S.debateWidthOffset(index))
    }

    val currentTurnOrResult = debate.currentTurn
    val isUsersTurn = role.exists(r =>
      currentTurnOrResult
        .unorderedFoldMap(_.rolesRemaining)
        .contains(r)
    )
    val charLimit = currentTurnOrResult.fold(_ => -1, _.charLimit)
    val quoteLimitOpt = currentTurnOrResult.fold(_ => None, _.quoteLimit)

    def makeSimpleVdomFromText(text: String) = {
      <.span(
        text
          .split("\n")
          .toVector
          .map(x => Vector(<.span(x): VdomElement))
          .intercalate(Vector(<.br()))
          .toVdomArray
      )
    }

    def breakNewlines(x: String) = {
      x.split("\n")
        .toVector
        .map(seg => Vector[VdomNode](<.span(seg)))
        .intercalate(Vector(<.br()))
        .toVdomArray
    }

    def minSecTime(millis: Long): String = {
      val secs = millis / 1000
      val mins = secs / 60
      val secsRem = secs % 60
      s"${mins}m ${secsRem}s"
    }

    def timestampHTML(timestamp: Long) = {
      debate.startTime.whenDefined { startTime =>
        val relTime = timestamp - startTime
        val humanReadableTimeUTC = {
          Instant
            .ofEpochMilli(timestamp)
            // TODO this should perhaps display it in the client's timezone
            .atZone(
              ZoneId.of("Z")
            ) // see "time zones" on http://cquiroz.github.io/scala-java-time/
            .toLocalTime
            .toString
        }
        <.span(S.speechTimestamp)(
          TagMod(
            minSecTime(relTime),
            " into the debate at "
          ).when(relTime > 0),
          humanReadableTimeUTC + " UTC"
        )
      }
    }

    def speechToHTML(speech: DebateSpeech) = {
      val roleString = speech.speaker.role.toString
      <.div(S.speechHeader)(
        speech.speaker.name,
        s" ($roleString) ",
        timestampHTML(speech.timestamp).when(
          userId
            .map(_.role)
            .collect { case Facilitator | Debater(_) => () }
            .nonEmpty
        )
      )
    }

    def quoteToHTML(span: ESpan) = {
      <.span(
        <.span(S.quoteText)(
          breakNewlines(
            Helpers.renderSpan(setup.sourceMaterial.contents, span)
          )
        ),
        <.span(S.quoteCitation)(s" (${span.begin}–${span.end})")
      )
    }

    def makeRoundHtml(
        round: DebateRound,
        roundIndex: Int
    ) = {
      <.div(
        ^.key := s"round-$roundIndex",
        round
          .timestamp(setup.numDebaters)
          .whenDefined(timestampHTML)
          .when(
            userId
              .map(_.role)
              .collect { case Facilitator | Debater(_) => () }
              .isEmpty
          ),
        round match {
          case SimultaneousSpeeches(speeches) =>
            if (speeches.size < setup.answers.size) {
              role
                .collect { case Debater(index) =>
                  speeches.get(index).map { speech =>
                    val speechStyle = TagMod(
                      S.answerOutline(index),
                      S.pendingBg,
                      S.debateWidthOffset(index)
                    )
                    makeSpeechHtml(speech, speechStyle)
                  }
                }
                .flatten
                .toVector
                .toVdomArray
            } else {
              Vector(
                makeSimultaneousSpeechesHtml(speeches)
              ).toVdomArray
            }
          case SequentialSpeeches(speeches) =>
            val speechesToShow = if (speeches.size < setup.answers.size) {
              if (
                role.collect { case Facilitator | Debater(_) => () }.nonEmpty
              ) {
                speeches.toVector.sortBy(_._1).map(_._2)
              } else Vector()
            } else speeches.values.toVector

            speechesToShow.toVdomArray { case speech =>
              val speechStyle = speech.speaker.role match {
                case Facilitator => TagMod(S.facilitatorBg)
                case Observer    => TagMod(S.observerBg)
                case Judge       => TagMod(S.judgeFeedbackBg)
                case Debater(index) =>
                  TagMod(S.answerBg(index), S.debateWidthOffset(index))
              }
              makeSpeechHtml(speech, speechStyle)
            }
          case JudgeFeedback(probabilities, speech, endsDebate) =>
            val speechStyle = speech.speaker.role match {
              case Facilitator => TagMod(S.facilitatorBg)
              case Observer    => TagMod(S.observerBg)
              case Judge =>
                TagMod(S.judgeFeedbackBg, S.judgeDecision.when(endsDebate))
              case Debater(index) =>
                TagMod(S.answerBg(index), S.debateWidthOffset(index))
            }
            Vector(
              Option(makeSpeechHtml(speech, speechStyle)),
              Option(
                <.div(
                  ^.display := "flex",
                  ^.flexDirection := "row",
                  probabilities.zipWithIndex.toVdomArray { case (prob, index) =>
                    val pct = f"${prob * 100.0}%.0f%%"
                    <.div(
                      S.answerBg(index),
                      ^.width := pct,
                      ^.color := "white",
                      ^.fontWeight := "bold",
                      ^.flexGrow := "1"
                    )(pct)
                  }
                )
              ).filter(_ => probabilities.size > 1)
            ).flatten.toVdomArray
        }
      )

    }

    def makeSimultaneousSpeechesHtml(
        speeches: Map[Int, DebateSpeech]
    ) = {
      <.div(S.speechRow)(
        speeches.toVector.sortBy(_._1).toVdomArray {
          case (debaterIndex, speech) =>
            <.div(S.speechBox, S.answerBg(debaterIndex))(
              ^.key := s"speech-$debaterIndex",
              speechToHTML(speech),
              speech.content.toVdomArray {
                case SpeechSegment.Text(text)  => makeSimpleVdomFromText(text)
                case SpeechSegment.Quote(span) => quoteToHTML(span)
              }
            )
        }
      )
    }

    def makeSpeechHtml(
        speech: DebateSpeech,
        style: TagMod
        // speechIndex: Int
    ) = {
      <.div(S.speechBox, style)(
        speechToHTML(speech),
        speech.content.toVdomArray {
          case SpeechSegment.Text(text)  => makeSimpleVdomFromText(text)
          case SpeechSegment.Quote(span) => quoteToHTML(span)
        }
      )
    }
    def getSpeechLength(speechSegments: Vector[SpeechSegment]) = {
      speechSegments.foldMap {
        case SpeechSegment.Text(text) => text.size
        case SpeechSegment.Quote(span) =>
          Helpers.renderSpan(setup.sourceMaterial.contents, span).size
      }
    }
    def getQuoteLength(speechSegments: Vector[SpeechSegment]) = {
      speechSegments.foldMap {
        case SpeechSegment.Text(_) => 0
        case SpeechSegment.Quote(span) =>
          Helpers.renderSpan(setup.sourceMaterial.contents, span).size
      }
    }

    val scrollDebateToBottom = Callback {
      val newSpeechesJQ = jQuery("#speeches")
      val newSpeechesDiv = newSpeechesJQ(0)
      newSpeechesJQ.scrollTop(
        newSpeechesDiv.scrollHeight - newSpeechesDiv.clientHeight
      )
    }
    def maybeScrollDebateToBottom = {
      val speechesJQ = jQuery("#speeches")
      val speechesDiv = speechesJQ(0)
      val isScrolledToBottom =
        speechesDiv.scrollHeight - speechesJQ.scrollTop() - speechesJQ
          .outerHeight() < 1
      if (isScrolledToBottom) scrollDebateToBottom else Callback.empty
    }

    val debateSpansWithSpeaker = rounds.flatMap { round =>
      if (round.isComplete(setup.answers.size)) {
        round.allSpeeches.view
          .flatMap(speech => speech.allQuotes.map(speech.speaker -> _))
          .toVector
      } else {
        userId
          .map(_.role)
          .view
          .flatMap(role => round.allSpeeches.filter(_.speaker.role == role))
          .flatMap(speech => speech.allQuotes.map(speech.speaker -> _))
          .toVector
      }
    }

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
            SpeechSegment.getSegmentsFromString(currentMessage.value)

          val speechLength = getSpeechLength(currentMessageSpeechSegments)
          val speechIsTooLong = charLimit > 0 && speechLength > charLimit
          val quoteLength = getQuoteLength(currentMessageSpeechSegments)
          val quotesAreTooLong = quoteLimitOpt.exists(quoteLength > _)
          val canSubmit = isUsersTurn && !speechIsTooLong && !quotesAreTooLong

          def speechInputPanel(
              submit: Boolean => Callback,
              cmdEnterToSubmit: Boolean
          ) = {
            <.div(S.speechInputPanel)(
              V.LiveTextArea.String.mod(
                textarea = TagMod(
                  S.fullWidthInput,
                  ^.rows := 5,
                  ^.onKeyDown ==> ((e: ReactKeyboardEvent) => {
                    val submitCB =
                      if (
                        cmdEnterToSubmit &&
                        e.keyCode == KeyCode.Enter && (e.metaKey || e.ctrlKey)
                      ) {
                        submit(false)
                      } else Callback.empty
                    submitCB
                  })
                )
              )(
                currentMessage,
                didUpdateValue = _ => maybeScrollDebateToBottom
              ),
              <.div(
                S.speechLengthPanel,
                S.invalidTextBackground.when(speechIsTooLong || quotesAreTooLong)
              )(
                "Length: ",
                <.span(S.speechLengthPanelOverage.when(speechIsTooLong))(
                  speechLength.toString
                ),
                <.span(" / ", charLimit.toString).when(charLimit > 0),
                ". Quote length: ",
                <.span(S.speechLengthPanelOverage.when(quotesAreTooLong))(
                  quoteLength.toString
                ),
                quoteLimitOpt.whenDefined { quoteLimit =>
                  <.span(" / ", quoteLimit.toString)
                }
              )
            )
          }

          def undoButtonPanel(submit: Boolean => Callback) = {
            <.button(
              "Undo",
              ^.onClick --> submit(true),
              ^.disabled := isUsersTurn // TODO(julianmichael): I think this is unnecessary
            )
          }

          def handleSimultaneousSpeechesWhenUsersTurn() = {
            val submit =
              (
                if (!canSubmit) Callback.empty
                else
                  userId.foldMap(userId =>
                    CallbackTo(System.currentTimeMillis()).flatMap { time =>
                      val speech = DebateSpeech(
                        userId,
                        time,
                        currentMessageSpeechSegments
                      )
                      val newRounds = role match {
                        case Some(Debater(debaterIndex)) =>
                          rounds.lastOption match {
                            // If we're in the middle of a simultaneous speech round that hasn't finished yet, add/update our speech to the turn
                            case Some(SimultaneousSpeeches(speeches))
                                if speeches.size < setup.answers.size =>
                              rounds.updated(
                                rounds.size - 1,
                                SimultaneousSpeeches(
                                  speeches + (debaterIndex -> speech)
                                )
                              )
                            // otherwise, create the record for the turn with our new speech.
                            case _ =>
                              rounds :+ SimultaneousSpeeches(
                                Map(debaterIndex -> speech)
                              )
                          }
                        case _ =>
                          rounds
                      }
                      sendDebate(Debate(setup, newRounds))
                    } >> currentMessage.setState("")
                  )
              )

            <.div(S.col)(
              speechInputPanel(_ => submit, true),
              <.button(
                "Submit",
                ^.disabled := !canSubmit,
                ^.onClick --> submit
              )
            )
          }

          def handleSequentialSpeechesWhenUsersTurn() = {
            val submitNewSpeech =
              (
                if (!canSubmit) Callback.empty
                else
                  userId.foldMap(userId =>
                    CallbackTo(System.currentTimeMillis()).flatMap { time =>
                      val speech = DebateSpeech(
                        userId,
                        time,
                        currentMessageSpeechSegments
                      )
                      val newRounds = role match {
                        case Some(Debater(debaterIndex)) =>
                          rounds.lastOption match {
                            // If we're in the middle of a simultaneous speech round that hasn't finished yet, add/update our speech to the turn
                            case Some(SequentialSpeeches(speeches))
                                if speeches.size < setup.answers.size =>
                              rounds.updated(
                                rounds.size - 1,
                                SequentialSpeeches(
                                  speeches + (debaterIndex -> speech)
                                )
                              )
                            // otherwise, create the record for the turn with our new speech.
                            case _ =>
                              rounds :+ SequentialSpeeches(
                                Map(debaterIndex -> speech)
                              )
                          }
                        case _ => rounds // TODO should we error here?
                      }
                      sendDebate(Debate(setup, newRounds))
                    } >> currentMessage.setState("")
                  )
              )

            <.div(S.col)(
              <.div(S.col)(
                speechInputPanel(
                  _ => submitNewSpeech,
                  cmdEnterToSubmit = true
                ),
                <.button(
                  "Submit",
                  ^.disabled := !canSubmit,
                  ^.onClick --> submitNewSpeech
                )
              )
            )
          }

          def undoSequentialSpeech(speeches: Map[Int, DebateSpeech]) = {
            val newLastRound = SequentialSpeeches(
              speeches - (speeches.size - 1)
            )
            rounds.dropRight(1) :+ newLastRound
          }

          def undoSimultaneousSpeech(
              speeches: Map[Int, DebateSpeech],
              participantID: ParticipantId
          ) = {
            val answerIndex = participantID.role match {
              case Debater(debaterIndex) =>
                debaterIndex
              case _ =>
                throw new RuntimeException(
                  "How did we get here? Somehow trying to undo a simultaneous speech for a non-debater"
                )
            }
            val newLastRound = SimultaneousSpeeches(
              speeches - answerIndex
            )
            rounds.dropRight(1) :+ newLastRound
          }

          def undoButtonWhenCurrentlyOnSequentialSpeech() = {
            val undoLastSpeech =
              userId.foldMap((_: ParticipantId) => {
                val newRounds =
                  rounds.lastOption match {
                    case Some(SequentialSpeeches(speeches))
                        if speeches.size > 0 =>
                      undoSequentialSpeech(speeches)
                    case Some(_: JudgeFeedback) =>
                      rounds.dropRight(1)
                    case _ =>
                      rounds
                  }
                sendDebate(
                  Debate(setup, rounds = newRounds)
                )

              })

            <.div(S.col)(
              undoButtonPanel(_ => undoLastSpeech)
            )
          }

          // TODO maybe-someday share this code with [undoButtonWhenCurrentlyOnSequentialSpeech]
          def undoButtonWhenCurrentlyOnSimultaneousSpeech() = {
            val undoLastSpeech =
              userId.foldMap((participantID: ParticipantId) => {
                val newRounds =
                  rounds.lastOption match {
                    case Some(SimultaneousSpeeches(speeches))
                        if speeches.size > 0 =>
                      undoSimultaneousSpeech(speeches, participantID)
                    case Some(_: JudgeFeedback) =>
                      rounds.dropRight(1)
                    case _ =>
                      rounds // TODO maybe-someday should we also catch sequential speeches? to handle all debate variants?
                  }

                sendDebate(
                  Debate(setup, rounds = newRounds)
                )
              })

            <.div(S.col)(
              undoButtonPanel(_ => undoLastSpeech)
            )
          }

          val isUndoAllowed = role
            .map { role =>
              debate.whoCanUndo.contains(role)
            }
            .getOrElse(false)

          def undoButtonWhenCurrentlyOnJudgeFeedback() = {
            val undoLastSpeech =
              userId.foldMap((participantID: ParticipantId) => {
                val newRounds =
                  rounds.lastOption match {
                    case Some(SequentialSpeeches(speeches))
                        if speeches.size > 0 =>
                      undoSequentialSpeech(speeches)
                    case Some(SimultaneousSpeeches(speeches))
                        if speeches.size > 0 =>
                      undoSimultaneousSpeech(speeches, participantID)
                    case Some(_: JudgeFeedback) =>
                      rounds.dropRight(1)
                    case _ =>
                      rounds
                  }
                sendDebate(
                  Debate(setup, rounds = newRounds)
                )

              })

            <.div(S.col)(
              undoButtonPanel(_ => undoLastSpeech)
            )
          }

          <.div(S.debateSubpanel)(
            <.div(S.speechesSubpanel)(
              ^.id := "speeches",
              rounds.zipWithIndex.flatMap { case (round, roundIndex) =>
                Option(makeRoundHtml(round, roundIndex)).filter(_ =>
                  role.collect { case Facilitator | Debater(_) =>
                    ()
                  }.nonEmpty ||
                    round.isComplete(setup.answers.size)
                )
              }.toVdomArray,
              userId.whenDefined { userId =>
                makeSpeechHtml(
                  DebateSpeech(userId, -1L, currentMessageSpeechSegments),
                  inProgressSpeechStyle
                ).when(currentMessage.value.size > 0 && isUsersTurn)
              }
            ),
            turnDisplay(role, currentTurnOrResult),
            currentTurnOrResult.toOption
              .whenDefined {
                case _: DebateTurnType.SimultaneousSpeechesTurn
                    if isUndoAllowed =>
                  undoButtonWhenCurrentlyOnSimultaneousSpeech()
                case _: DebateTurnType.DebaterSpeechTurn if isUndoAllowed =>
                  undoButtonWhenCurrentlyOnSequentialSpeech()
                case _: DebateTurnType.JudgeFeedbackTurn if isUndoAllowed =>
                  undoButtonWhenCurrentlyOnJudgeFeedback()
                case _ =>
                  <.div()()
              },
            currentTurnOrResult.toOption.filter(_ => isUsersTurn).whenDefined {
              case DebateTurnType.SimultaneousSpeechesTurn(
                    _: Set[Int],
                    _: Int,
                    _: Option[Int]
                  ) =>
                handleSimultaneousSpeechesWhenUsersTurn()

              case DebateTurnType.DebaterSpeechTurn(_, _, _) =>
                handleSequentialSpeechesWhenUsersTurn()

              case DebateTurnType.JudgeFeedbackTurn(
                    _: Boolean,
                    _: Int
                  ) =>
                val turnNum =
                  debate.rounds.collect { case JudgeFeedback(_, _, _) =>
                    1
                  }.sum
                LocalProbs.make(
                  Vector.fill(setup.answers.size)(1.0 / setup.answers.size)
                ) { probs =>
                  def submit(endDebate: Boolean) =
                    (
                      if (!isUsersTurn || speechIsTooLong) Callback.empty
                      else
                        userId.foldMap(userId =>
                          CallbackTo(System.currentTimeMillis()).flatMap(time =>
                            sendDebate(
                              Debate(
                                setup,
                                rounds :+ JudgeFeedback(
                                  probs.value,
                                  DebateSpeech(
                                    userId,
                                    time,
                                    Vector(
                                      SpeechSegment.Text(currentMessage.value)
                                    )
                                  ),
                                  endDebate
                                )
                              )
                            )
                          ) >> currentMessage.setState("")
                        )
                    )

                  LocalBool.make(false) { consideringContinue =>
                    val barWidthPx = 60

                    <.div(S.row)(
                      <.div(S.grow)(
                        ProbabilitySliders(probs) {
                          case ProbabilitySliders
                                .Context(index, prob, setProb) =>
                            <.div(S.row)(
                              <.span(S.answerProbLabel(index))(
                                f"${prob * 100.0}%.0f%% ${answerLetter(index)}. "
                              ),
                              <.input(S.probSlider(index))(
                                ^.`type` := "range",
                                ^.min := 0,
                                ^.max := 1,
                                ^.step := 0.01,
                                ^.value := prob,
                                ^.onChange ==> ((e: ReactEventFromInput) => {
                                  setProb(e.target.value.toDouble)
                                })
                              )
                            )
                        },
                        speechInputPanel(submit, false),
                        <.div(S.row)(
                          <.button(S.grow)(
                            "End debate & collect reward",
                            ^.disabled := !isUsersTurn || speechIsTooLong,
                            ^.onClick --> submit(endDebate = true)
                          ),
                          <.button(S.grow)(
                            f"Continue debate for $$${setup.rules.scoringFunction.perTurnPenalty}%.2f",
                            ^.disabled := !isUsersTurn || speechIsTooLong,
                            ^.onMouseMove --> consideringContinue
                              .setState(true),
                            ^.onMouseLeave --> consideringContinue
                              .setState(false),
                            ^.onClick --> submit(endDebate = false)
                          )
                        )
                      ),
                      <.div(
                        S.col,
                        ^.width := s"${barWidthPx * setup.answers.size}px"
                      ) {
                        val currentScores = setup.answers.indices.map(index =>
                          setup.rules.scoringFunction
                            .eval(turnNum, probs.value, index)
                        )
                        val hypotheticalScores =
                          setup.answers.indices.map(index =>
                            setup.rules.scoringFunction
                              .eval(turnNum + 1, probs.value, index)
                          )

                        val reqdDeltas =
                          ScoringFunction.deltasForNextTurnToBeWorthwhile(
                            setup.rules.scoringFunction,
                            probs.value,
                            turnNum
                          )

                        val scores =
                          if (consideringContinue.value) hypotheticalScores
                          else currentScores

                        val maxNegMagnitude = 20.0
                        val min =
                          math.min(0, math.max(-maxNegMagnitude, scores.min))
                        val range = setup.rules.scoringFunction.max - min
                        val maxRelScore =
                          math.abs(setup.rules.scoringFunction.max) / range
                        val maxNegRelScore =
                          math.min(maxNegMagnitude, math.abs(min)) / range
                        TagMod(
                          <.div(S.col, S.grow)(
                            <.div(S.row)(
                              setup.answers.indices.zip(scores).toVdomArray {
                                case (index, _) =>
                                  <.div(S.col, ^.width := s"${barWidthPx}px")(
                                    reqdDeltas(index).whenDefined(delta =>
                                      <.span(f"+${delta * 100}%.0f%%")
                                    )
                                  )
                              }
                            ).when(consideringContinue.value),
                            <.div(
                              S.row,
                              ^.height := f"${maxRelScore * 100}%.2f%%"
                            )(
                              setup.answers.indices.zip(scores).toVdomArray {
                                case (index, score) =>
                                  val relScore = math.abs(score) / range
                                  <.div(S.col, ^.width := s"${barWidthPx}px")(
                                    <.div(
                                      S.col,
                                      S.grow,
                                      ^.justifyContent := "flex-end"
                                    )(
                                      ^.position := "relative",
                                      <.div(f"$$${score}%.2f")
                                        .when(relScore <= 0.10),
                                      <.div(
                                        S.answerBg(index),
                                        ^.height := f"${relScore / maxRelScore * 100}%.2f%%",
                                        <.div(f"$$${score}%.2f")
                                          .when(relScore > 0.10)
                                      )
                                    ).when(score >= 0.0)
                                  )
                              }
                            ),
                            <.div(
                              S.row,
                              ^.height := f"${maxNegRelScore * 100}%.2f%%"
                            )(
                              setup.answers.indices.zip(scores).toVdomArray {
                                case (index, score) =>
                                  val relScore = math.min(
                                    maxNegMagnitude,
                                    math.abs(score)
                                  ) / range
                                  <.div(S.col, ^.width := s"${barWidthPx}px")(
                                    <.div(
                                      S.col,
                                      S.grow,
                                      ^.justifyContent := "flex-start"
                                    )(
                                      ^.position := "relative",
                                      <.div(
                                        S.col,
                                        ^.justifyContent := "flex-end"
                                      )(
                                        S.answerBg(index),
                                        ^.height := f"${relScore / maxNegRelScore * 100}%.2f%%",
                                        <.div(f"$$${score}%.2f")
                                          .when(relScore < -0.10)
                                      ),
                                      <.div(f"$$${score}%.2f")
                                        .when(relScore >= -0.10)
                                    ).when(score < 0.0)
                                  )
                              }
                            )
                          ),
                          <.div(S.row)(
                            setup.answers.indices.toVdomArray { case index =>
                              <.div(
                                S.inputRowItem,
                                ^.width := s"${barWidthPx}px"
                              )(answerLetter(index))
                            }
                          )
                        )
                      }
                    )
                  }
                }
            }
          )
        }
      )
    }
  }
}
