package debate
package view.debate

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._

import jjm.DotPair

import debate.util.Local

object SpeechInput {
  // import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  import Utils.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  // assumes it's the user's turn
  def speechInput(
    debate: StateSnapshot[Debate],
    userName: String,
    role: Role,
    turn: DotPair[* => Debate, DebateTurnType],
    currentMessage: StateSnapshot[String]
  ): TagMod = {
    val setup                        = debate.value.setup
    val currentMessageSpeechSegments = SpeechSegments.getFromString(currentMessage.value)
    val source                       = debate.value.setup.sourceMaterial.contents

    val charLimitOpt = turn.fst.charLimitOpt
    val speechLength = SpeechSegments.getSpeechLength(source, currentMessageSpeechSegments)
    val speechIsTooLong = charLimitOpt
      .exists(charLimit => charLimit > 0 && speechLength > charLimit)

    val quoteLimitOpt    = turn.fst.quoteLimit
    val quoteLength      = SpeechSegments.getQuoteCoverage(source, currentMessageSpeechSegments)
    val quotesAreTooLong = quoteLimitOpt.exists(quoteLength > _)

    val globalQuoteLimitOpt = debate.value.setup.rules.globalQuoteRestriction
    val globalQuoteNumChars = SpeechSegments.getQuoteCoverage(
      source,
      currentMessageSpeechSegments ++
        debate
          .value
          .rounds
          .foldMap(_.allSpeeches.filter(_._1 == role).values.toVector.foldMap(_.content))
    )
    val totalQuoteLimitExceeded = globalQuoteLimitOpt.exists(globalQuoteNumChars > _)

    val canSubmit = !speechIsTooLong && !quotesAreTooLong && !totalQuoteLimitExceeded

    def speechInputPanel(submit: Boolean => Callback, cmdEnterToSubmit: Boolean) =
      <.div(S.speechInputPanel)(
        V.LiveTextArea
          .String
          .mod(textarea =
            TagMod(
              S.fullWidthInput,
              ^.rows := 5,
              ^.onKeyDown ==>
                ((e: ReactKeyboardEvent) => {
                  val submitCB =
                    if (
                      cmdEnterToSubmit && e.keyCode == KeyCode.Enter && (e.metaKey || e.ctrlKey)
                    ) {
                      submit(false)
                    } else
                      Callback.empty
                  submitCB
                })
            )
          )(currentMessage),
        <.div(
          S.speechLengthPanel,
          S.invalidTextBackground.when(speechIsTooLong || quotesAreTooLong)
        )(
          "Length: ",
          <.span(S.speechLengthPanelOverage.when(speechIsTooLong))(speechLength.toString),
          charLimitOpt.map(charLimit => <.span(" / ", charLimit.toString)),
          ReactFragment(
            ". Quote length: ",
            <.span(S.speechLengthPanelOverage.when(quotesAreTooLong))(quoteLength.toString),
            quoteLimitOpt.map { quoteLimit =>
              <.span(" / ", quoteLimit.toString)
            },
            ". Quote total: ",
            <.span(S.speechLengthPanelOverage.when(totalQuoteLimitExceeded))(
              globalQuoteNumChars.toString
            ),
            globalQuoteLimitOpt.map { quoteLimit =>
              <.span(" / ", quoteLimit.toString)
            }
          ).when(role != Judge)
        )
      )

    //   _ => submitNewSpeech,
    //   cmdEnterToSubmit = true
    // <.button(
    //   "Submit",
    //   ^.disabled := !canSubmit,
    //   (^.onClick --> submitNewSpeech).when(canSubmit)
    // ),

    def debaterSpeechInput(
      // turnType: DebateTurnType { type Input = DebateSpeechContent },
      giveSpeech: DebateSpeech => Debate
    ) = {
      def submit = CallbackTo(System.currentTimeMillis()).flatMap(time =>
        debate.setState(giveSpeech(DebateSpeech(userName, time, currentMessageSpeechSegments))) >>
          currentMessage.setState("")
      )
      <.div(S.col)(
        speechInputPanel(_ => submit, true),
        <.button("Submit", ^.disabled := !canSubmit, (^.onClick --> submit).when(canSubmit))
      )
    }

    def judgeSpeechInput(
      turnType: DebateTurnType.JudgeFeedbackTurn,
      giveSpeech: JudgeFeedback => Debate
    ) = {
      val turnNum =
        debate
          .value
          .rounds
          .collect { case JudgeFeedback(_, _, _) =>
            1
          }
          .sum
      Local[Vector[Double]].make(Vector.fill(setup.answers.size)(1.0 / setup.answers.size)) {
        probs =>
          def submit(endDebate: Boolean) =
            if (speechIsTooLong)
              Callback.empty
            else
              CallbackTo(System.currentTimeMillis()).flatMap(time =>
                debate.setState(
                  giveSpeech(
                    JudgeFeedback(
                      distribution = probs.value,
                      feedback = DebateSpeech(
                        speaker = userName,
                        timestamp = time,
                        content = currentMessageSpeechSegments
                      ),
                      endDebate = endDebate
                    )
                  )
                )
              ) >> currentMessage.setState("")

          Local[Boolean].make(false) { consideringContinue =>
            val barWidthPx = 60

            <.div(S.row)(
              <.div(S.grow)(
                ProbabilitySliders(probs) { case ProbabilitySliders.Context(index, prob, setProb) =>
                  <.div(S.row)(
                    <.span(S.answerProbLabel(index))(
                      f"${prob * 100.0}%.0f%% ${answerLetter(index)}. "
                    ),
                    <.input(S.probSlider(index))(
                      ^.`type` := "range",
                      ^.min    := 0,
                      ^.max    := 1,
                      ^.step   := 0.01,
                      ^.value  := prob,
                      ^.onChange ==> ((e: ReactEventFromInput) => setProb(e.target.value.toDouble))
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
                      ^.onMouseMove --> consideringContinue.setState(true),
                      ^.onMouseLeave --> consideringContinue.setState(false),
                      ^.onClick --> submit(endDebate = false)
                    )
                    .when(!turnType.mustEndDebate)
                )
              ),
              <.div(S.col, ^.width := s"${barWidthPx * setup.answers.size}px") {
                val currentScores = setup
                  .answers
                  .indices
                  .map(index => setup.rules.scoringFunction.eval(turnNum, probs.value, index))
                val hypotheticalScores = setup
                  .answers
                  .indices
                  .map(index => setup.rules.scoringFunction.eval(turnNum + 1, probs.value, index))

                val reqdDeltas = ScoringFunction.deltasForNextTurnToBeWorthwhile(
                  setup.rules.scoringFunction,
                  probs.value,
                  turnNum
                )

                val scores =
                  if (consideringContinue.value)
                    hypotheticalScores
                  else
                    currentScores

                val maxNegMagnitude = 20.0
                val min             = math.min(0, math.max(-maxNegMagnitude, scores.min))
                val range           = setup.rules.scoringFunction.max - min
                val maxRelScore     = math.abs(setup.rules.scoringFunction.max) / range
                val maxNegRelScore  = math.min(maxNegMagnitude, math.abs(min)) / range
                TagMod(
                  <.div(S.col, S.grow)(
                    <.div(S.row)(
                        setup
                          .answers
                          .indices
                          .zip(scores)
                          .toVdomArray { case (index, _) =>
                            <.div(S.col, ^.width := s"${barWidthPx}px")(
                              reqdDeltas(index)
                                .whenDefined(delta => <.span(f"+${delta * 100}%.0f%%"))
                            )
                          }
                      )
                      .when(consideringContinue.value),
                    <.div(S.row, ^.height := f"${maxRelScore * 100}%.2f%%")(
                      setup
                        .answers
                        .indices
                        .zip(scores)
                        .toVdomArray { case (index, score) =>
                          val relScore = math.abs(score) / range
                          <.div(S.col, ^.width := s"${barWidthPx}px")(
                            <.div(S.col, S.grow, ^.justifyContent := "flex-end")(
                                ^.position := "relative",
                                <.div(f"$$$score%.2f").when(relScore <= 0.10),
                                <.div(
                                  S.answerBg(index),
                                  ^.height := f"${relScore / maxRelScore * 100}%.2f%%",
                                  <.div(f"$$$score%.2f").when(relScore > 0.10)
                                )
                              )
                              .when(score >= 0.0)
                          )
                        }
                    ),
                    <.div(S.row, ^.height := f"${maxNegRelScore * 100}%.2f%%")(
                      setup
                        .answers
                        .indices
                        .zip(scores)
                        .toVdomArray { case (index, score) =>
                          val relScore = math.min(maxNegMagnitude, math.abs(score)) / range
                          <.div(S.col, ^.width := s"${barWidthPx}px")(
                            <.div(S.col, S.grow, ^.justifyContent := "flex-start")(
                                ^.position := "relative",
                                <.div(S.col, ^.justifyContent := "flex-end")(
                                  S.answerBg(index),
                                  ^.height := f"${relScore / maxNegRelScore * 100}%.2f%%",
                                  <.div(f"$$$score%.2f").when(relScore < -0.10)
                                ),
                                <.div(f"$$$score%.2f").when(relScore >= -0.10)
                              )
                              .when(score < 0.0)
                          )
                        }
                    )
                  ),
                  <.div(S.row)(
                    setup
                      .answers
                      .indices
                      .toVdomArray { case index =>
                        <.div(S.inputRowItem, ^.width := s"${barWidthPx}px")(answerLetter(index))
                      }
                  )
                )
              }
            )
          }
      }
    }

    def negotiateEndInput(vote: Boolean => Debate) =
      <.div(S.speechInputPanel)(
        "Offer to end the debate?",
        <.div(c"row")(
          <.button(c"col-md-6 btn btn-primary")(
            "Continue",
            ^.onClick --> debate.setState(vote(false))
          ),
          <.button(c"col-md-6 btn btn-danger")("End", ^.onClick --> debate.setState(vote(false)))
        )
      )

    turn.fst match {
      case turnType @ DebateTurnType.SimultaneousSpeechesTurn(_, _, _) =>
        debaterSpeechInput(turn.get(turnType).get)
      case turnType @ DebateTurnType.DebaterSpeechTurn(_, _, _) =>
        debaterSpeechInput(turn.get(turnType).get)
      case turnType @ DebateTurnType.JudgeFeedbackTurn(_, _, _) =>
        judgeSpeechInput(turnType, turn.get(turnType).get)
      case turnType @ DebateTurnType.NegotiateEndTurn(_) =>
        negotiateEndInput(turn.get(turnType).get)
    }
  }
}
