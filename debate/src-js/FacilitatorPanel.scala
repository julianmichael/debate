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

import monocle.function.{all => Optics}

import cats.implicits._

class FacilitatorPanel(
    val S: Styles.type,
    val V: jjm.ui.View
) {

  import App.ClassSetInterpolator

  val RoundTypeList =
    ListConfig[DebateRoundType](DebateRoundType.SequentialSpeechesRound(500))
  val RoundTypeConfig = SumConfig[DebateRoundType]()
  val ScoringFunctionConfig = SumConfig[ScoringFunction]()
  val DebateSetupSpecLocal = new LocalState[DebateSetupSpec]

  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  val QuALITYIndexFetch = new CacheCallContent[Unit, Map[String, String]]
  val QuALITYStoryOptFetch =
    new CacheCallContent[Option[String], Option[QuALITYStory]]
  val QuALITYStorySelect = new V.Select[Option[(String, String)]](
    show = choice =>
      choice match {
        case None                     => "(custom)"
        case Some((articleId, title)) => s"$title ($articleId)"
      }
  )

  val QuestionOptLocal = new LocalState[Option[QuALITYQuestion]]
  val QuestionSelect = new V.Select[Option[QuALITYQuestion]](
    show = choice =>
      choice match {
        case None           => "(custom)"
        case Some(question) => question.question
      }
  )

  val ProfileOptSelect = new V.OptionalSelect[String](show = _.toString)

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

  def roundTypeConfig(
      label: String,
      rounds: StateSnapshot[Vector[DebateRoundType]],
      minItems: Int
  ) = {
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputLabel)(label),
      roundTypeSelect(rounds, minItems)
    )
  }

  def scoringFunctionConfig(scoringFunction: StateSnapshot[ScoringFunction]) = {
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputLabel)("Judge Scoring Function"),
      ScoringFunctionConfig.mod(div = S.inputRowContents)(scoringFunction)(
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
      // roundTypeSelect(setup.zoomStateL(DebateSetupSpec.rules.composeLens(DebateRules.repeatingStructure)), 1),
    )
  }

  /** Config panel for facilitator to set the rules of the debate. */
  def apply(
      mustAssignRoles: Boolean,
      profiles: Set[String],
      qualityService: QuALITYService[AsyncCallback],
      sendUpdate: DebateStateUpdateRequest => Callback
  ) = DebateSetupSpecLocal.make(DebateSetupSpec.init) { setup =>
    val canStartDebate = setup.value.answers.filter(_.nonEmpty).size > 1 && (
      !mustAssignRoles || setup.value.areAllRolesAssigned
    )

    QuALITYIndexFetch.make(
      request = (),
      sendRequest = _ => OrWrapped.wrapped(qualityService.getIndex)
    ) { indexFetch =>
      val indexOpt = indexFetch match {
        case QuALITYIndexFetch.Loading       => None
        case QuALITYIndexFetch.Loaded(index) => Some(index)
      }
      def getTitle(articleId: String): String =
        indexOpt.flatMap(_.get(articleId)).getOrElse("(loading title)")
      val articleIdOpt = setup.value.sourceMaterial match {
        case CustomSourceMaterialSpec(_, _)       => None
        case QuALITYSourceMaterialSpec(articleId) => Some(articleId)
      }
      QuALITYStoryOptFetch.make(
        request = articleIdOpt,
        sendRequest = articleIdOpt =>
          articleIdOpt match {
            case None => OrWrapped.pure[AsyncCallback](None)
            case Some(articleId) =>
              OrWrapped.wrapped(
                qualityService.getStory(articleId).map(Option(_))
              )
          }
      ) { storyOptFetch =>
        val storyOpt = storyOptFetch.toOption.flatten
        QuestionOptLocal.make(None) { qualityQuestionOpt =>
          <.div(S.facilitatorColumn)(
            roundTypeConfig(
              "Opening Rounds",
              setup.zoomStateL(
                DebateSetupSpec.rules.composeLens(DebateRules.fixedOpening)
              ),
              minItems = 0
            ),
            roundTypeConfig(
              "Repeated Rounds",
              setup.zoomStateL(
                DebateSetupSpec.rules.composeLens(
                  DebateRules.repeatingStructure
                )
              ),
              minItems = 1
            ),
            scoringFunctionConfig(
              setup.zoomStateL(
                DebateSetupSpec.rules.composeLens(DebateRules.scoringFunction)
              )
            ),
            <.div(S.mainLabeledInputRow)(
              <.div(S.inputLabel)("Source Material"),
              <.div(S.inputRowContents)(
                QuALITYStorySelect.modFull(select = S.customSelect)(
                  choices = None +: indexOpt
                    .foldMap(_.toList.sortBy(_.swap))
                    .map(Some(_)),
                  curChoice = articleIdOpt.map(id => id -> getTitle(id)),
                  setChoice = choice =>
                    choice match {
                      case None =>
                        setup
                          .zoomStateL(DebateSetupSpec.sourceMaterial)
                          .setState(
                            CustomSourceMaterialSpec(
                              "Title",
                              "Custom source material."
                            )
                          )
                      case Some((articleId, _)) => {
                        setup
                          .zoomStateL(DebateSetupSpec.sourceMaterial)
                          .setState(
                            QuALITYSourceMaterialSpec(articleId)
                          )
                      }
                    }
                ),
                setup.value.sourceMaterial match {
                  case CustomSourceMaterialSpec(_, _) =>
                    val customSourceMaterialSpec = setup
                      .zoomStateO(
                        DebateSetupSpec.sourceMaterial
                          .composePrism(SourceMaterialSpec.custom)
                      )
                      .get // will succeed bc of match case
                    VdomArray(
                      // title
                      V.LiveTextField.String.modInput(input = c"form-control")(
                        customSourceMaterialSpec.zoomStateL(
                          CustomSourceMaterialSpec.title
                        )
                      ),
                      // article
                      V.LiveTextArea.String
                        .mod(textarea = TagMod(c"form-control", ^.rows := 10))(
                          customSourceMaterialSpec.zoomStateL(
                            CustomSourceMaterialSpec.contents
                          ),
                          placeholderOpt = Some("Paste source material here")
                        )
                    )
                  case QuALITYSourceMaterialSpec(articleId) =>
                    VdomArray(
                      <.input(
                        c"form-control",
                        ^.readOnly := true,
                        ^.value := getTitle(articleId)
                      ),
                      <.textarea(
                        c"form-control",
                        ^.rows := 10,
                        ^.readOnly := true,
                        ^.value := storyOpt.fold("(loading story contents)")(
                          _.article
                        )
                      )
                    )
                }
              )
            ),
            <.div(S.mainLabeledInputRow)(
              <.div(S.inputLabel)("Question"),
              <.div(S.inputRowContents)(
                QuestionSelect.modFull(select = S.customSelect)(
                  choices = None +: storyOpt
                    .foldMap(_.questions.values.toList)
                    .map(Some(_)),
                  curChoice = qualityQuestionOpt.value,
                  setChoice = {
                    case None =>
                      qualityQuestionOpt.setState(None) >>
                        setup.modState(
                          DebateSetupSpec.question.set("") andThen
                            DebateSetupSpec.answers.set(Vector("", ""))
                        )
                    case Some(question) =>
                      import io.circe.syntax._
                      Callback(
                        org.scalajs.dom.console.log(question.asJson.spaces2)
                      ) >>
                        qualityQuestionOpt.setState(Some(question)) >>
                        setup.modState(setup =>
                          setup.copy(
                            question = question.question,
                            answers = question.options,
                            correctAnswerIndex = question.annotations.fold(
                              setup.correctAnswerIndex
                            )(_.writerLabel - 1) // writer labels are 1-indexed
                          )
                        )
                  }
                ),
                V.LiveTextField.String(
                  setup.zoomStateL(DebateSetupSpec.question)
                ),
                <.div(S.row)(
                  <.div(S.inputLabel)("Assigned Judge:"),
                  ProfileOptSelect.mod(select = S.customSelect)(
                    choices = profiles,
                    choice = setup.zoomStateL(
                      DebateSetupSpec.roles.composeLens(
                        Optics.at(Judge: DebateRole)
                      )
                    )
                  )
                )
              )
            ),
            <.div(S.mainLabeledInputRow)(
              <.div(S.inputLabel)("Answers"),
              <.div(S.inputRowContents)(
                ListConfig.String(
                  setup.zoomStateL(DebateSetupSpec.answers),
                  1
                ) { (remove, answer, index) =>
                  <.div(S.row)(
                    <.span(S.answerLabel)(
                      remove,
                      " ",
                      s"${answerLetter(index)}. "
                    ),
                    <.div(S.col, S.grow)(
                      <.div(S.row)(
                        V.LiveTextField.String(answer)
                      ),
                      <.div(S.row)(
                        <.input(S.correctAnswerRadio)(
                          ^.`type` := "radio",
                          ^.name := "correctAnswerIndex",
                          ^.value := index,
                          ^.checked := setup.value.correctAnswerIndex == index,
                          ^.onChange --> setup
                            .zoomStateL(DebateSetupSpec.correctAnswerIndex)
                            .setState(index)
                        ),
                        <.span(S.inputRowItem)(
                          <.span(S.correctAnswerLabel)("Correct"),
                          S.hidden.when(setup.value.correctAnswerIndex != index)
                        ),
                        <.div(S.inputLabel)("Assigned Debater:"),
                        ProfileOptSelect.mod(select = S.customSelect)(
                          choices = profiles,
                          choice = setup.zoomStateL(
                            DebateSetupSpec.roles.composeLens(
                              Optics.at(Debater(index): DebateRole)
                            )
                          )
                        )
                      )
                    )
                  )
                }
              )
            ),
            <.button(c"btn btn-primary")(
              "Start the debate!",
              ^.disabled := !canStartDebate,
              ^.onClick ==> ((e: ReactEvent) => {
                if (!canStartDebate) Callback.empty
                else {
                  e.preventDefault();
                  sendUpdate(DebateStateUpdateRequest.SetupSpec(setup.value))
                }
              })
            )
          )

        }

      }
    }
  }

}
