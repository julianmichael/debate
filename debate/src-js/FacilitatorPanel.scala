package debate
import debate.util._
import debate.quality._

// import org.scalajs.dom

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.MonocleReact._

// import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import jjm.ui.LocalState
import jjm.ui.CacheCallContent
import jjm.OrWrapped

class FacilitatorPanel(
  S: Styles.type,
  V: jjm.ui.View
) {
  val RoundTypeList =
      ListConfig[DebateRoundType](DebateRoundType.SequentialSpeechesRound(500))
  val RoundTypeConfig = SumConfig[DebateRoundType]()
  val ScoringFunctionConfig = SumConfig[ScoringFunction]()
  val DebateSetupSpecLocal = new LocalState[DebateSetupSpec]

  import scala.concurrent.ExecutionContext.Implicits.global

  val QuALITYIndexFetch = new CacheCallContent[Unit, Map[String, String]]
  val QuALITYStoryFetch = new CacheCallContent[String, QuALITYStory]
  private val QuALITYStorySelect = new V.Select[Option[(String, String)]](
    show = choice => choice match {
      case None => "(custom)"
      case Some((articleId, title)) => s"$title ($articleId)"
    }
  )
  val QuALITYStoryOptLocal = new LocalState[Option[(String, String)]]

  /** Config panel for setting a list of round types. */
  def roundTypeSelect(
      roundTypes: StateSnapshot[Vector[DebateRoundType]],
      minItems: Int
  ) = {
    RoundTypeList(roundTypes, minItems) { (remove, roundType, _) =>
      <.div( // (S.labeledInputRow)
        <.span(S.inputRowItem)(remove, " "),
        RoundTypeConfig(roundType)(
          "Simultaneous Speeches" -> SumConfigOption(
            DebateRoundType.SimultaneousSpeechesRound(500),
            DebateRoundType.simultaneousSpeechesRound
          ) { simulSpeeches =>
            <.span(S.inputRowItem)(
              <.span("Character limit"),
              V.NumberField(
                simulSpeeches.zoomStateL(
                  DebateRoundType.SimultaneousSpeechesRound.charLimit
                )
              )
            )
          },
          "Sequential Speeches" -> SumConfigOption(
            DebateRoundType.SequentialSpeechesRound(500),
            DebateRoundType.sequentialSpeechesRound
          ) { seqSpeeches =>
            <.span(S.inputRowItem)(
              <.span("Character limit"),
              V.NumberField(
                seqSpeeches.zoomStateL(
                  DebateRoundType.SequentialSpeechesRound.charLimit
                )
              )
            )
          },
          "Judge Feedback" -> SumConfigOption(
            DebateRoundType.JudgeFeedbackRound(true, 500),
            DebateRoundType.judgeFeedbackRound
          ) { judgeFeedback =>
            <.span(S.inputRowItem)(
              V.Checkbox(
                judgeFeedback.zoomStateL(
                  DebateRoundType.JudgeFeedbackRound.reportBeliefs
                ),
                "Report probabilities"
              ),
              <.span(
                <.span("Character limit"),
                V.NumberField(
                  judgeFeedback.zoomStateL(
                    DebateRoundType.JudgeFeedbackRound.charLimit
                  )
                )
              )
            )
          }
        )
      )
    }
  }

  /** Config panel for facilitator to set the rules of the debate. */
  def apply(
    qualityService: QuALITYService[AsyncCallback],
    sendUpdate: DebateStateUpdateRequest => Callback
  ) = DebateSetupSpecLocal.make(DebateSetupSpec.init) { setupRaw =>
    val answers = setupRaw.value.answers
    <.div(S.debateColumn)(
      <.form(
        ^.onSubmit ==> (
          (e: ReactEvent) => {
            e.preventDefault();
            sendUpdate(DebateStateUpdateRequest.SetupSpec(setupRaw.value))
        }),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Opening Rounds"),
          roundTypeSelect(
            setupRaw.zoomStateL(
              DebateSetupSpec.rules.composeLens(DebateRules.fixedOpening)
            ),
            0
          )
        ),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Repeated Rounds"),
          roundTypeSelect(
            setupRaw.zoomStateL(
              DebateSetupSpec.rules.composeLens(DebateRules.repeatingStructure)
            ),
            1
          )
        ),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Judge Scoring Function"),
          ScoringFunctionConfig(
            setupRaw.zoomStateL(
              DebateSetupSpec.rules.composeLens(DebateRules.scoringFunction)
            )
          )(
            "Spherical Score" -> SumConfigOption(
              ScoringFunction.SphericalScoreWithLinearPenalty(3, 1),
              ScoringFunction.sphericalScoreWithLinearPenalty
            ) { sphericalScore =>
              <.span(S.inputRowItem)(
                V.LiveTextField.Double(
                  sphericalScore.zoomStateL(
                    ScoringFunction.SphericalScoreWithLinearPenalty.baseCoefficient
                  ),
                  Some("Base coefficient")
                ),
                V.LiveTextField.Double(
                  sphericalScore.zoomStateL(
                    ScoringFunction.SphericalScoreWithLinearPenalty.perTurnPenalty
                  ),
                  Some("Per turn penalty")
                )
              )
            },
            "Quadratic Score" -> SumConfigOption(
              ScoringFunction.QuadraticScoreWithLinearPenalty(3, 1),
              ScoringFunction.quadraticScoreWithLinearPenalty
            ) { quadraticScore =>
              <.span(S.inputRowItem)(
                V.LiveTextField.Double(
                  quadraticScore.zoomStateL(
                    ScoringFunction.QuadraticScoreWithLinearPenalty.baseCoefficient
                  ),
                  Some("Base coefficient")
                ),
                V.LiveTextField.Double(
                  quadraticScore.zoomStateL(
                    ScoringFunction.QuadraticScoreWithLinearPenalty.perTurnPenalty
                  ),
                  Some("Per turn penalty")
                )
              )
            },
            "Log Score" -> SumConfigOption(
              ScoringFunction.LogScoreWithLinearPenalty(3.0, 1.0, 2.0, 1.0),
              ScoringFunction.logScoreWithLinearPenalty
            ) { logScore =>
              <.span(S.inputRowItem)(
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.baseCoefficient
                  ),
                  Some("Base coefficient")
                ),
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.constant
                  ),
                  Some("Constant")
                ),
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.logBase
                  ),
                  Some("Log base")
                ),
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.perTurnPenalty
                  ),
                  Some("Per turn penalty")
                )
              )
            }
          )
          // roundTypeSelect(setupRaw.zoomStateL(DebateSetupSpec.rules.composeLens(DebateRules.repeatingStructure)), 1),
        ),
        QuALITYStoryOptLocal.make(None) { curStory =>
          <.div(S.labeledInputRow)(
            <.div(S.inputLabel)("Source Material"),
            QuALITYIndexFetch.make(request = (), sendRequest = _ => OrWrapped.wrapped(qualityService.getIndex)) {
              case QuALITYIndexFetch.Loading => <.div("Loading QuALITY story list...")
              case QuALITYIndexFetch.Loaded(index) =>
                val indexList = index.toList.sortBy(_._2)
                QuALITYStorySelect(None +: indexList.map(Some(_)), curStory)
            },
            curStory.value match {
              case None =>
                V.LiveTextArea.String
                  .mod(textarea = TagMod(S.fullWidthInput, ^.rows := 10))(
                    setupRaw.zoomStateL(DebateSetupSpec.sourceMaterial),
                    placeholderOpt = Some("Paste source material here")
                  )
              case Some((articleId, _)) =>
                QuALITYStoryFetch.make(request = articleId, sendRequest = articleId => OrWrapped.wrapped(qualityService.getStory(articleId))) {
                  case QuALITYStoryFetch.Loading => <.div("Loading QuALITY story...")
                  case QuALITYStoryFetch.Loaded(story) => 
                    <.textarea(
                      S.fullWidthInput,
                      ^.rows := 10,
                      ^.readOnly := true,
                      ^.value := story.article
                    )
                }
            }
          )
        },
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Question"),
          V.LiveTextField.String.mod(input = S.fullWidthInput)(
            setupRaw.zoomStateL(DebateSetupSpec.question)
          )
        ),
        <.div(S.labeledInputRow)(
          <.span(S.inputLabel)("Answers"),
          ListConfig.String(setupRaw.zoomStateL(DebateSetupSpec.answers), 1) {
            (remove, answer, index) =>
              <.div(S.labeledInputRow)(
                <.span(S.answerLabel)(remove, " ", s"${answerLetter(index)}. "),
                V.LiveTextField.String.mod(input = S.fullWidthInput)(answer),
                <.input(S.correctAnswerRadio)(
                  ^.`type` := "radio",
                  ^.name := "correctAnswerIndex",
                  ^.value := index,
                  ^.checked := setupRaw.value.correctAnswerIndex == index,
                  ^.onChange --> setupRaw
                    .zoomStateL(DebateSetupSpec.correctAnswerIndex)
                    .setState(index)
                ),
                <.span(S.inputRowItem)(
                  "Correct answer",
                  S.hidden.when(setupRaw.value.correctAnswerIndex != index)
                )
              )
          }
        ),
        <.button(
          "Start the debate!",
          ^.`type` := "submit",
          ^.disabled := answers.filter(_.nonEmpty).size < 2
        )
      )
    )
  }

}
