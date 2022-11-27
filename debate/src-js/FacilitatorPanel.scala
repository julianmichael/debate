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

import cats.implicits._

class FacilitatorPanel(
  S: Styles.type,
  V: jjm.ui.View
) {

  import App.ClassSetInterpolator

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

  /** Config panel for setting a list of round types. */
  def roundTypeSelect(
      roundTypes: StateSnapshot[Vector[DebateRoundType]],
      minItems: Int
  ) = {
    RoundTypeList.mod(
      listDiv = S.inputRowContents,
      addItemDiv = S.row
    )(roundTypes, minItems) { (remove, roundType, _) =>
      <.div(S.row)(
        <.span(remove, " "), // (S.inputRowItem)
        RoundTypeConfig.mod(div = S.inputRowContents)(roundType)(
          "Simultaneous Speeches" -> SumConfigOption(
            DebateRoundType.SimultaneousSpeechesRound(500),
            DebateRoundType.simultaneousSpeechesRound
          ) { simulSpeeches =>
            VdomArray(
              <.div(S.row)(
                <.div(S.inputLabel)("Character limit"),
                V.NumberField.mod(input = c"form-control")(
                  simulSpeeches.zoomStateL(
                    DebateRoundType.SimultaneousSpeechesRound.charLimit
                  )
                )
              )
            )
          },
          "Sequential Speeches" -> SumConfigOption(
            DebateRoundType.SequentialSpeechesRound(500),
            DebateRoundType.sequentialSpeechesRound
          ) { seqSpeeches =>
            VdomArray(
              <.div(S.row)(
                <.div(S.inputLabel)("Character limit"),
                V.NumberField.mod(input = c"form-control")(
                  seqSpeeches.zoomStateL(
                    DebateRoundType.SequentialSpeechesRound.charLimit
                  )
                )
              )
            )
          },
          "Judge Feedback" -> SumConfigOption(
            DebateRoundType.JudgeFeedbackRound(true, 500),
            DebateRoundType.judgeFeedbackRound
          ) { judgeFeedback =>
            VdomArray(
              <.div(S.row)(
                <.span(S.inputLabel)("Character limit"),
                V.NumberField.mod(input = c"form-control")(
                  judgeFeedback.zoomStateL(
                    DebateRoundType.JudgeFeedbackRound.charLimit
                  )
                )
              ),
              <.div(S.row)(
                V.Checkbox(
                  judgeFeedback.zoomStateL(
                    DebateRoundType.JudgeFeedbackRound.reportBeliefs
                  ),
                  "Report probabilities"
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
    <.div(S.facilitatorColumn)(
      <.div(S.mainLabeledInputRow)(
        <.div(S.inputLabel)("Opening Rounds"),
        roundTypeSelect(
          setupRaw.zoomStateL(
            DebateSetupSpec.rules.composeLens(DebateRules.fixedOpening)
          ),
          0
        )
      ),
      <.div(S.mainLabeledInputRow)(
        <.div(S.inputLabel)("Repeated Rounds"),
        <.div(S.inputRowContents)(
          roundTypeSelect(
            setupRaw.zoomStateL(
              DebateSetupSpec.rules.composeLens(DebateRules.repeatingStructure)
            ),
            1
          )
        )
      ),
      <.div(S.mainLabeledInputRow)(
        <.div(S.inputLabel)("Judge Scoring Function"),
        ScoringFunctionConfig.mod(
          div = S.inputRowContents)(
          setupRaw.zoomStateL(
            DebateSetupSpec.rules.composeLens(DebateRules.scoringFunction)
          )
        )(
          "Spherical Score" -> SumConfigOption(
            ScoringFunction.SphericalScoreWithLinearPenalty(3, 1),
            ScoringFunction.sphericalScoreWithLinearPenalty
          ) { sphericalScore =>
            VdomArray(
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
            VdomArray(
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
            ScoringFunction.LogScoreWithLinearPenalty.default,
            ScoringFunction.logScoreWithLinearPenalty
          ) { logScore =>
            VdomArray(
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
      QuALITYIndexFetch.make(
        request = (),
        sendRequest = _ => OrWrapped.wrapped(qualityService.getIndex)
      ) { indexFetch =>
        // val indexOpt = indexFetch.toOption
        val indexOpt = indexFetch match {
          case QuALITYIndexFetch.Loading => None
          case QuALITYIndexFetch.Loaded(index) => Some(index)

        }
        def getTitle(articleId: String): String = indexOpt.flatMap(_.get(articleId)).getOrElse("(loading title)")
        // val indexList = indexOpt.foldMap(_.toList)

        <.div(S.mainLabeledInputRow)(
          <.div(S.inputLabel)("Source Material"),
          <.div(S.inputRowContents)(
            QuALITYStorySelect.modFull(select = c"custom-select")(
              choices = None +: indexOpt.foldMap(_.toList.sortBy(_.swap)).map(Some(_)),
              curChoice = setupRaw.value.sourceMaterial match {
                case CustomSourceMaterialSpec(_, _) => None
                case QuALITYSourceMaterialSpec(articleId) => Some(
                  articleId -> getTitle(articleId)
                )
              },
              setChoice = choice => choice match {
                case None => setupRaw.zoomStateL(DebateSetupSpec.sourceMaterial).setState(
                  CustomSourceMaterialSpec("Title", "Custom source material.")
                )
                case Some((articleId, _)) => {
                  setupRaw.zoomStateL(DebateSetupSpec.sourceMaterial).setState(
                    QuALITYSourceMaterialSpec(articleId)
                  )
                }
              }
            ),
            setupRaw.value.sourceMaterial match {
              case CustomSourceMaterialSpec(_, _) =>
                val customSourceMaterialSpec = setupRaw.zoomStateO(
                  DebateSetupSpec.sourceMaterial.composePrism(SourceMaterialSpec.custom)
                ).get // will succeed bc of match case
                VdomArray(
                  // title
                  V.LiveTextField.String.modInput(input = c"form-control")(
                    customSourceMaterialSpec.zoomStateL(CustomSourceMaterialSpec.title)
                  ),
                  // article
                  V.LiveTextArea.String.mod(textarea = TagMod(c"form-control", ^.rows := 10))(
                    customSourceMaterialSpec.zoomStateL(CustomSourceMaterialSpec.contents),
                    placeholderOpt = Some("Paste source material here")
                  )
                )
              case QuALITYSourceMaterialSpec(articleId) =>
                VdomArray(
                  // <.input(S.fullWidthInput, ^.rows := 10, ^.readOnly)(
                  //   getTitleOpt(spec.articleId).getOrElse("(loading title)")
                  // )
                  <.input(
                    c"form-control",
                    ^.readOnly := true,
                    ^.value := getTitle(articleId)
                  ),
                  QuALITYStoryFetch.make(
                    request = articleId,
                    sendRequest = articleId => OrWrapped.wrapped(qualityService.getStory(articleId))
                  ) { storyFetch =>
                    val storyOpt = storyFetch match {
                      case QuALITYStoryFetch.Loading => None
                      case QuALITYStoryFetch.Loaded(story) => Some(story)
                    }
                      <.textarea(
                        c"form-control",
                        ^.rows := 10,
                        ^.readOnly := true,
                        ^.value := storyOpt.fold("(loading story contents)")(_.article)
                      )
                  }
                )
            }
          )
        )
      },
      <.div(S.mainLabeledInputRow)(
        <.div(S.inputLabel)("Question"),
        <.div(S.inputRowContents)(
          V.LiveTextField.String(
            setupRaw.zoomStateL(DebateSetupSpec.question)
          )
        )
      ),
      <.div(S.mainLabeledInputRow)(
        <.div(S.inputLabel)("Answers"),
        <.div(S.inputRowContents)(
          ListConfig.String(setupRaw.zoomStateL(DebateSetupSpec.answers), 1) {
            (remove, answer, index) =>
              <.div(S.row)(
                <.span(S.answerLabel)(remove, " ", s"${answerLetter(index)}. "),
                V.LiveTextField.String(answer),
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
        )
      ),
      <.button(c"btn btn-primary")(
        "Start the debate!",
        ^.disabled := answers.filter(_.nonEmpty).size < 2, // TODO factor this out
        ^.onClick ==> (
          (e: ReactEvent) => {
            if(answers.filter(_.nonEmpty).size < 2) Callback.empty else {
              e.preventDefault();
              sendUpdate(DebateStateUpdateRequest.SetupSpec(setupRaw.value))
            }
          }
        )
      )
    )
  }

}
