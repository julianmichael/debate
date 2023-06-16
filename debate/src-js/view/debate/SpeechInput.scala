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
import jjm.implicits._

import debate.util.Local
import debate.answerLetter

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
    currentMessage: StateSnapshot[String],
    saveCallbackOpt: Option[Callback]
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

    def speechInputPanel(submit: Callback, cmdEnterToSubmit: Boolean) =
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
                      submit
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

    def judgingInputPanel(
      probs: StateSnapshot[Vector[Double]],
      numContinues: Int,
      submit: Callback,
      continueOpt: Option[Callback],
      saveOpt: Option[Callback]
    ) =
      Local[Boolean].make(false) { consideringContinue =>
        val answerIndices = setup.answers.indices
        val currentScores = answerIndices
          .map(setup.rules.scoringFunction.eval(numContinues, probs.value, _))
        val hypotheticalScores = answerIndices
          .map(setup.rules.scoringFunction.eval(numContinues + 1, probs.value, _))
        val reqdDeltas = ScoringFunction
          .deltasForNextTurnToBeWorthwhile(setup.rules.scoringFunction, probs.value, numContinues)
        val scores =
          if (consideringContinue.value)
            hypotheticalScores
          else
            currentScores
        val range = math.max(scores.max, 0) - math.min(scores.min, 0)

        <.div(S.row)(
          <.div(S.grow)(
            ProbabilitySliders(probs) { case ProbabilitySliders.Context(index, prob, setProb) =>
              <.div(S.row)(
                <.span(S.answerProbLabel(index))(f"${prob * 100.0}%.0f%% ${answerLetter(index)}. "),
                <.input(S.probSlider(index))(
                  ^.`type` := "range",
                  ^.min    := 0.01,
                  ^.max    := 0.99,
                  ^.step   := 0.01,
                  ^.value  := prob,
                  ^.onChange ==> ((e: ReactEventFromInput) => setProb(e.target.value.toDouble))
                )
              )
            },
            speechInputPanel(submit, false),
            Local[Boolean].make(false) { showConfirmationDialog =>
              <.div(S.row)(
                <.div(S.globalDialogOverlay)(
                    <.div(S.popupDialog, c"card")(
                      <.div(c"card-header")("End the debate?"),
                      <.div(c"card-body")(
                        <.p("You are about to end the debate with the following choice: "),
                        probs.value.zipWithIndex.maximaBy(_._1) match {
                          case Nil =>
                            <.p(c"text-danger")("Error: no answers.")
                          case (prob, index) :: Nil =>
                            <.p(S.answerBox(index))(
                              <.span(S.answerProbLabel(index))(
                                f"(${prob * 100}%.0f%%) ${answerLetter(index)}: "
                              ),
                              debate.value.setup.answers(index)
                            )
                          case multiple =>
                            val letters =
                              Utils
                                .delimitedSpans(multiple.map(p => answerLetter(p._2)))
                                .toVdomArray
                            <.p(c"text-muted", S.bold)(s"Uncertain (equal between ", letters, ").")
                        },
                        <.p("Proceed?"),
                        <.button(c"card-button card-link btn btn-outline-danger")(
                          "Cancel",
                          ^.onClick --> showConfirmationDialog.setState(false)
                        ),
                        <.button(c"card-button card-link btn btn-primary")(
                          "Submit",
                          ^.onClick --> (showConfirmationDialog.setState(false) >> submit)
                        )
                      )
                    )
                  )
                  .when(showConfirmationDialog.value),
                saveOpt.whenDefined(save =>
                  <.button(c"btn btn-secondary")(
                    <.i(c"bi bi-arrow-left"),
                    " Save",
                    ^.onClick --> save
                  )
                ),
                <.button(S.grow)(
                  "Submit judgment & collect reward",
                  ^.disabled := speechIsTooLong,
                  ^.onClick --> showConfirmationDialog.setState(true)
                ),
                continueOpt.map { continue =>
                  // TODO: calculate full penalty based on number of debate rounds in between
                  <.button(S.grow)(
                    f"Continue debate for $$${setup.rules.scoringFunction.perTurnPenalty}%.2f",
                    ^.disabled := speechIsTooLong,
                    ^.onMouseMove --> consideringContinue.setState(true),
                    ^.onMouseLeave --> consideringContinue.setState(false),
                    ^.onClick --> continue
                  )
                }
              )
            }
          ),
          <.div(S.row)(
            scores
              .zipWithIndex
              .toVdomArray { case (score, index) =>
                val relScore = math.abs(score) / range
                val justify =
                  if (score >= scores.max)
                    "flex-start"
                  else
                    "flex-end"
                val (scoreTopMarg, scoreBotMarg) =
                  if (score > 0)
                    ("initial", "auto")
                  else
                    ("auto", "initial")
                val deltaPerc = reqdDeltas(index).map(d => f"+${d * 100}%.0f%%").getOrElse("")
                <.div(S.col, ^.alignItems := "center")(
                  <.div(
                    ^.fontStyle := "italic",
                    if (consideringContinue.value)
                      deltaPerc
                    else
                      "\u00A0"
                  ), // unicode non-breaking space (&nbsp;)
                  <.div(S.grow, S.col, ^.justifyContent := justify)(
                    <.div(
                      S.col,
                      S.answerBg(index),
                      ^.alignItems := "center",
                      ^.height     := f"${relScore * 100}%.2f%%",
                      ^.margin     := "0",
                      ^.minWidth   := "6em",
                      <.div(
                        S.answerBg(index),
                        ^.textAlign    := "center",
                        ^.marginTop    := scoreTopMarg,
                        ^.marginBottom := scoreBotMarg,
                        ^.color        := "white",
                        ^.minWidth     := "4em",
                        f"$$$score%.2f"
                      )
                    )
                  ),
                  <.div(^.fontWeight := "bold", ^.marginTop := "2em", answerLetter(index))
                )
              }
          )
        )
      }

    def debaterSpeechInput(
      // turnType: DebateTurnType { type Input = DebateSpeechContent },
      saveOpt: Option[Callback],
      giveSpeech: DebateSpeech => Debate
    ) = {
      def submit = CallbackTo(System.currentTimeMillis()).flatMap(time =>
        debate.setState(giveSpeech(DebateSpeech(userName, time, currentMessageSpeechSegments))) >>
          currentMessage.setState("")
      )
      <.div(S.col)(
        speechInputPanel(submit, true),
        <.div(S.row)(
          saveOpt.whenDefined(save =>
            <.button(c"btn btn-secondary")(<.i(c"bi bi-arrow-left"), " Save", ^.onClick --> save)
          ),
          <.button(c"btn btn-primary")(
            "Submit",
            ^.disabled := !canSubmit,
            (^.onClick --> submit).when(canSubmit)
          )
        )
      )
    }

    def judgeSpeechInput(
      saveOpt: Option[Callback],
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

          val finish      = submit(true)
          val continueOpt = Option(submit(false)).filter(_ => !turnType.mustEndDebate)

          judgingInputPanel(probs, turnNum, finish, continueOpt, saveOpt)
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
          <.button(c"col-md-6 btn btn-danger")("End", ^.onClick --> debate.setState(vote(true)))
        )
      )

    def offlineJudgingInput(
      turnType: DebateTurnType.OfflineJudgingTurn,
      giveSpeech: ((String, OfflineJudgment)) => Debate
    ) = {
      def startJudgingButton(mode: OfflineJudgingMode) = <
        .button(c"btn btn-block btn-primary")(
          s"Judge ($mode)",
          ^.onClick -->
            debate.setState(
              giveSpeech(userName -> OfflineJudgment(mode, System.currentTimeMillis(), Vector()))
            )
        )
        .when(
          mode ==
            OfflineJudgingMode.Stepped
            // debate.value.setup.offlineJudges.get(userName).flatten.forall(_ == mode)
        )

      turnType.existingJudgments.get(userName) match {
        case None =>
          <.div(S.spaceyContainer)(
            startJudgingButton(OfflineJudgingMode.Timed),
            startJudgingButton(OfflineJudgingMode.Stepped)
          )
        case Some(j @ OfflineJudgment(_, _, _)) if j.result.nonEmpty =>
          EmptyVdom
        case Some(OfflineJudgment(mode, startTimeMillis, judgments)) =>
          val numContinues = judgments.size // we know result is nonempty, so all are continues
          val numTurns     = debate.value.numDebateRounds

          Local[Vector[Double]].make(Vector.fill(setup.answers.size)(1.0 / setup.answers.size)) {
            probs =>
              def submit(endDebate: Boolean) =
                if (speechIsTooLong)
                  Callback.empty
                else
                  CallbackTo(System.currentTimeMillis()).flatMap(time =>
                    debate.setState(
                      giveSpeech(
                        userName ->
                          OfflineJudgment(
                            mode,
                            startTimeMillis = startTimeMillis,
                            judgments =
                              judgments :+
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
                    )
                  ) >> currentMessage.setState("")

              val continueOpt = Option(submit(endDebate = false))
                .filter(_ => numContinues < numTurns)

              judgingInputPanel(
                probs,
                numContinues,
                submit(endDebate = true),
                continueOpt,
                saveOpt = None
              )
          }
      }
    }

    turn.fst match {
      case turnType @ DebateTurnType.SimultaneousSpeechesTurn(_, _, _) =>
        debaterSpeechInput(saveCallbackOpt, turn.get(turnType).get)
      case turnType @ DebateTurnType.DebaterSpeechTurn(_, _, _) =>
        debaterSpeechInput(saveCallbackOpt, turn.get(turnType).get)
      case turnType @ DebateTurnType.JudgeFeedbackTurn(_, _, _) =>
        judgeSpeechInput(saveCallbackOpt, turnType, turn.get(turnType).get)
      case turnType @ DebateTurnType.NegotiateEndTurn(_) =>
        negotiateEndInput(turn.get(turnType).get)
      case turnType @ DebateTurnType.OfflineJudgingTurn(_) =>
        offlineJudgingInput(turnType, turn.get(turnType).get)
    }
  }
}
