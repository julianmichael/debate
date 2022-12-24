package debate

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._

import jjm.DotPair
import jjm.ui.LocalState

object SpeechInput {
  // import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  // import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  val LocalProbs = new LocalState[Vector[Double]]
  val LocalBool = new LocalState[Boolean]

  // assumes it's the user's turn
    def speechInput(
      debate: Debate,
      sendDebate: Debate => Callback,
      userId: Option[ParticipantId],
      turn: DotPair[* => Debate, DebateTurnType],
      currentMessage: StateSnapshot[String]
    ): TagMod = {
      val setup = debate.setup
      val currentMessageSpeechSegments =
        SpeechSegments.getFromString(currentMessage.value)
      val source = debate.setup.sourceMaterial.contents

      val charLimit = turn.fst.charLimit
      val quoteLimitOpt = turn.fst.quoteLimit
      val speechLength = SpeechSegments.getSpeechLength(source, currentMessageSpeechSegments)
      val speechIsTooLong = charLimit > 0 && speechLength > charLimit
      val quoteLength = SpeechSegments.getQuoteLength(source, currentMessageSpeechSegments)
      val quotesAreTooLong = quoteLimitOpt.exists(quoteLength > _)
      val canSubmit = !speechIsTooLong && !quotesAreTooLong
      // val canSubmit = isUsersTurn && !speechIsTooLong && !quotesAreTooLong

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
          )(currentMessage),
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

      //   _ => submitNewSpeech,
      //   cmdEnterToSubmit = true
      // <.button(
      //   "Submit",
      //   ^.disabled := !canSubmit,
      //   (^.onClick --> submitNewSpeech).when(canSubmit)
      // ),

      def debaterSpeechInput(
        // turnType: DebateTurnType { type Input = DebateSpeechContent },
        giveSpeech: DebateSpeechContent => Debate
      ) = {
        def submit = userId.foldMap(userId =>
          CallbackTo(System.currentTimeMillis()).flatMap(time =>
            sendDebate(
              giveSpeech(DebateSpeechContent(userId.name, time, currentMessageSpeechSegments))
            ) >> currentMessage.setState("")
          )
        )
        <.div(S.col)(
          speechInputPanel(_ => submit, true),
          <.button(
            "Submit",
            ^.disabled := !canSubmit,
            (^.onClick --> submit).when(canSubmit)
          )
        )
      }

      def judgeSpeechInput(
        // turnType: DebateTurnType.JudgeFeedbackTurn,
        giveSpeech: JudgeFeedbackContent => Debate
      ) = {
        val turnNum =
          debate.rounds.collect { case JudgeFeedback(_, _, _) =>
            1
          }.sum
        LocalProbs.make(
          Vector.fill(setup.answers.size)(1.0 / setup.answers.size)
        ) { probs =>
          def submit(endDebate: Boolean) =
            (
              if (speechIsTooLong) Callback.empty
              else
                userId.foldMap(userId =>
                  CallbackTo(System.currentTimeMillis()).flatMap(time =>
                    sendDebate(
                      giveSpeech(
                        JudgeFeedbackContent(
                          speakerName = userId.name,
                          timestamp = time,
                          speech = currentMessageSpeechSegments,
                          distribution = probs.value,
                          endDebate = endDebate
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
                    ^.disabled := speechIsTooLong,
                    ^.onClick --> submit(endDebate = true)
                  ),
                  <.button(S.grow)(
                    f"Continue debate for $$${setup.rules.scoringFunction.perTurnPenalty}%.2f",
                    ^.disabled := speechIsTooLong,
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

      turn.fst match {
        case turnType @ DebateTurnType.SimultaneousSpeechesTurn(_, _, _) =>
          debaterSpeechInput(turn.get(turnType).get)
        case turnType @ DebateTurnType.DebaterSpeechTurn(_, _, _) =>
          debaterSpeechInput(turn.get(turnType).get)
        case turnType @ DebateTurnType.JudgeFeedbackTurn(_, _) =>
          judgeSpeechInput(turn.get(turnType).get)
      }
    }
}
