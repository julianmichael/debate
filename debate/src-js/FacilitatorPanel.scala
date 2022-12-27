package debate

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Iso
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.OrWrapped
import jjm.ui.CacheCallContent
import jjm.ui.LocalState

import debate.quality._
import debate.util._
import cats.data.NonEmptyChain
import cats.data.Validated

object FacilitatorPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Helpers.ClassSetInterpolator

  val RoundTypeList = ListConfig[DebateRoundType](
    DebateRoundType.SequentialSpeechesRound(500, None)
  )
  val RoundTypeConfig       = SumConfig[DebateRoundType]()
  val ScoringFunctionConfig = SumConfig[ScoringFunction]()
  val DebateSetupSpecLocal  = new LocalState2[DebateSetupSpec]
  val BoolLocal             = new LocalState2[Boolean]
  val StringLocal           = new LocalState2[String]

  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  val QuALITYIndexFetch    = new CacheCallContent[Unit, Map[String, String]]
  val QuALITYStoryOptFetch = new CacheCallContent[Option[String], Option[QuALITYStory]]
  val QuALITYStorySelect =
    new V.Select[Option[(String, String)]](
      show =
        choice =>
          choice match {
            case None =>
              "(custom)"
            case Some((articleId, title)) =>
              s"$title ($articleId)"
          }
    )

  val QuestionOptLocal = new LocalState[Option[QuALITYQuestion]]
  val QuestionSelect =
    new V.Select[Option[QuALITYQuestion]](
      show =
        choice =>
          choice match {
            case None =>
              "(custom)"
            case Some(question) =>
              question.question
          }
    )

  val ProfileOptSelect = new V.OptionalSelect[String](show = _.toString)

  val defaultPerMessageQuoteLimit = 100
  val defaultGlobalQuoteLimit     = 500

  def optionalIntInput(
    field: StateSnapshot[Option[Int]],
    labelOpt: Option[String],
    defaultInt: Int
  ) = ReactFragment(
    V.Checkbox
      .mod(label = S.inputLabel, span = TagMod(c"ml-3 pl-3 my-auto"))(
        field.zoomStateL(
          Iso[Option[Int], Boolean](_.nonEmpty)(b =>
            if (b)
              Some(defaultInt)
            else
              None
          ).asLens
        ),
        labelOpt
      ),
    V.NumberField
      .mod(input = TagMod(c"form-control", ^.disabled := field.value.isEmpty))(
        field.zoomStateL(Iso[Option[Int], Int](_.getOrElse(defaultInt))(Some(_)).asLens)
      )
  )

  /** Config panel for setting a list of round types. */
  def roundTypeSelect(roundTypes: StateSnapshot[Vector[DebateRoundType]], minItems: Int) =
    RoundTypeList.mod(listDiv = S.inputRowContents, addItemDiv = S.row)(roundTypes, minItems) {
      (remove, roundType, _) =>
        <.div(S.row)(
          <.span(remove, " "), // (S.inputRowItem)
          RoundTypeConfig.mod(div = S.inputRowContents)(roundType)(
            "Simultaneous Speeches" ->
              SumConfigOption(
                DebateRoundType.SimultaneousSpeechesRound(500, None),
                DebateRoundType.simultaneousSpeechesRound
              ) { simulSpeeches =>
                ReactFragment(
                  <.div(S.row)(
                    <.div(S.inputLabel)("Character limit"),
                    V.NumberField
                      .mod(input = c"form-control")(
                        simulSpeeches
                          .zoomStateL(DebateRoundType.SimultaneousSpeechesRound.charLimit)
                      )
                  ),
                  <.div(S.row)(
                    optionalIntInput(
                      simulSpeeches
                        .zoomStateL(DebateRoundType.SimultaneousSpeechesRound.quoteLimit),
                      Some("Quote character limit"),
                      defaultPerMessageQuoteLimit
                    )
                  )
                )
              },
            "Sequential Speeches" ->
              SumConfigOption(
                DebateRoundType.SequentialSpeechesRound(500, None),
                DebateRoundType.sequentialSpeechesRound
              ) { seqSpeeches =>
                ReactFragment(
                  <.div(S.row)(
                    <.div(S.inputLabel)("Character limit"),
                    V.NumberField
                      .mod(input = c"form-control")(
                        seqSpeeches.zoomStateL(DebateRoundType.SequentialSpeechesRound.charLimit)
                      )
                  ),
                  <.div(S.row)(
                    optionalIntInput(
                      seqSpeeches.zoomStateL(DebateRoundType.SequentialSpeechesRound.quoteLimit),
                      Some("Quote character limit"),
                      defaultPerMessageQuoteLimit
                    )
                  )
                )
              },
            "Judge Feedback" ->
              SumConfigOption(
                DebateRoundType.JudgeFeedbackRound(true, 500),
                DebateRoundType.judgeFeedbackRound
              ) { judgeFeedback =>
                ReactFragment(
                  <.div(S.row)(
                    <.span(S.inputLabel)("Character limit"),
                    V.NumberField
                      .mod(input = c"form-control")(
                        judgeFeedback.zoomStateL(DebateRoundType.JudgeFeedbackRound.charLimit)
                      )
                  ),
                  <.div(S.row)(
                    V.Checkbox(
                      judgeFeedback.zoomStateL(DebateRoundType.JudgeFeedbackRound.reportBeliefs),
                      "Report probabilities"
                    )
                  )
                )
              }
          )
        )
    }

  def roomSettings(
    isOfficial: StateSnapshot[Boolean],
    roomName: StateSnapshot[String],
    createDebateOpt: Option[Callback]
  ) = ReactFragment(
    Helpers.textInputWithEnterButton(
      field = roomName,
      placeholderOpt = Some("Room name"),
      buttonText = "Create",
      isEnabled = createDebateOpt.nonEmpty,
      enter = createDebateOpt.combineAll
    ),
    <.div(S.row)(V.Checkbox(isOfficial, "Is an official debate (must assign roles)"))
  )

  def roundTypeConfig(
    label: String,
    rounds: StateSnapshot[Vector[DebateRoundType]],
    minItems: Int
  ) = <.div(S.mainLabeledInputRow)(<.div(S.inputLabel)(label), roundTypeSelect(rounds, minItems))

  def scoringFunctionConfig(scoringFunction: StateSnapshot[ScoringFunction]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputLabel)("Judge Scoring Function"),
      ScoringFunctionConfig.mod(div = S.inputRowContents)(scoringFunction)(
        "Spherical Score" ->
          SumConfigOption(
            ScoringFunction.SphericalScoreWithLinearPenalty(3, 1),
            ScoringFunction.sphericalScoreWithLinearPenalty
          ) { sphericalScore =>
            ReactFragment(
              V.LiveTextField
                .Double(
                  sphericalScore
                    .zoomStateL(ScoringFunction.SphericalScoreWithLinearPenalty.baseCoefficient),
                  Some("Base coefficient")
                ),
              V.LiveTextField
                .Double(
                  sphericalScore
                    .zoomStateL(ScoringFunction.SphericalScoreWithLinearPenalty.perTurnPenalty),
                  Some("Per turn penalty")
                )
            )
          },
        "Quadratic Score" ->
          SumConfigOption(
            ScoringFunction.QuadraticScoreWithLinearPenalty(3, 1),
            ScoringFunction.quadraticScoreWithLinearPenalty
          ) { quadraticScore =>
            ReactFragment(
              V.LiveTextField
                .Double(
                  quadraticScore
                    .zoomStateL(ScoringFunction.QuadraticScoreWithLinearPenalty.baseCoefficient),
                  Some("Base coefficient")
                ),
              V.LiveTextField
                .Double(
                  quadraticScore
                    .zoomStateL(ScoringFunction.QuadraticScoreWithLinearPenalty.perTurnPenalty),
                  Some("Per turn penalty")
                )
            )
          },
        "Log Score" ->
          SumConfigOption(
            ScoringFunction.LogScoreWithLinearPenalty.default,
            ScoringFunction.logScoreWithLinearPenalty
          ) { logScore =>
            ReactFragment(
              V.LiveTextField
                .Double(
                  logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.baseCoefficient),
                  Some("Base coefficient")
                ),
              V.LiveTextField
                .Double(
                  logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.constant),
                  Some("Constant")
                ),
              V.LiveTextField
                .Double(
                  logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.logBase),
                  Some("Log base")
                ),
              V.LiveTextField
                .Double(
                  logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.perTurnPenalty),
                  Some("Per turn penalty")
                )
            )
          }
      )
    )

  def globalQuoteRestrictionConfig(quoteRestriction: StateSnapshot[Option[Int]]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputLabel)("Global Evidence Restrictions"),
      <.div(S.inputRowContents)(
        <.div(S.row)(
          optionalIntInput(quoteRestriction, Some("Quote character limit"), defaultGlobalQuoteLimit)
        )
      )
    )

  /** Config panel for facilitator to set the rules of the debate. */
  def apply(
    lobby: Lobby,
    joinDebate: Option[(Boolean, String) => Callback],
    profiles: Set[String],
    qualityService: QuALITYService[AsyncCallback],
    initDebate: CreateRoom => Callback
  ) = {
    DebateSetupSpecLocal.syncedWithSessionStorage("debate-setup", DebateSetupSpec.init) { setup =>
      StringLocal.syncedWithSessionStorage("debate-setup-room-name", "") { roomName =>
        BoolLocal.syncedWithSessionStorage("debate-setup-is-official", true) { isOfficial =>
          val createDebateValidated: Validated[NonEmptyChain[Option[VdomTag]], Callback] = {
            import cats.implicits._
            // import cats.syntax.validated._
            val createDebateCb: Callback =
              initDebate(
                CreateRoom(
                  isOfficial = isOfficial.value,
                  roomName = roomName.value.trim,
                  setupSpec = setup.value
                )
              ) >> roomName.setState("")

            def ensure(key: String, condition: Boolean, err: Option[VdomTag]) =
              condition
                .validNec[Option[VdomTag]]
                .ensure(NonEmptyChain(err.map(_(c"text-danger", ^.key := key))))(identity)

            ensure("room name is not blank", roomName.value.nonEmpty, None) *>
              ensure(
                "at least 2 answers",
                setup.value.answers.filter(_.nonEmpty).size > 1,
                Some(<.div("Debates must have at least 2 answers."))
              ) *>
              ensure(
                "all roles assigned",
                !isOfficial.value || setup.value.areAllRolesAssigned,
                Some(<.div("All roles must be assigned in official debates."))
              ) *>
              ensure(
                "room doesn't exist",
                if (isOfficial.value)
                  !lobby.officialRooms.exists(_.name == roomName.value.trim)
                else
                  !lobby.practiceRooms.exists(_.name == roomName.value.trim),
                Some {
                  val prefix =
                    if (isOfficial.value)
                      "official"
                    else
                      "practice"
                  <.div(
                    s"The $prefix room ",
                    <.a(
                      roomName.value.trim,
                      ^.href := "#",
                      joinDebate
                        .whenDefined(cb => ^.onClick --> cb(isOfficial.value, roomName.value))
                    ),
                    " already exists."
                  )
                }
              ) *> createDebateCb.validNec[Option[VdomTag]]
          }

          QuALITYIndexFetch
            .make(request = (), sendRequest = _ => OrWrapped.wrapped(qualityService.getIndex)) {
              indexFetch =>
                val indexOpt =
                  indexFetch match {
                    case QuALITYIndexFetch.Loading =>
                      None
                    case QuALITYIndexFetch.Loaded(index) =>
                      Some(index)
                  }
                def getTitle(articleId: String): String = indexOpt
                  .flatMap(_.get(articleId))
                  .getOrElse("(loading title)")
                val articleIdOpt =
                  setup.value.sourceMaterial match {
                    case CustomSourceMaterialSpec(_, _) =>
                      None
                    case QuALITYSourceMaterialSpec(articleId) =>
                      Some(articleId)
                  }
                QuALITYStoryOptFetch.make(
                  request = articleIdOpt,
                  sendRequest =
                    articleIdOpt =>
                      articleIdOpt match {
                        case None =>
                          OrWrapped.pure[AsyncCallback](None)
                        case Some(articleId) =>
                          OrWrapped.wrapped(qualityService.getStory(articleId).map(Option(_)))
                      }
                ) { storyOptFetch =>
                  val storyOpt = storyOptFetch.toOption.flatten
                  QuestionOptLocal.make(None) { qualityQuestionOpt =>
                    <.div(S.facilitatorColumn, S.spaceySubcontainer)(
                      <.div(S.spaceySubcontainer, S.stickyBanner)(
                        roomSettings(isOfficial, roomName, createDebateValidated.toOption),
                        <.div {
                          val (prefix, rooms) =
                            if (isOfficial.value)
                              "official" -> lobby.officialRooms
                            else
                              "practice" -> lobby.practiceRooms

                          <.span(
                              s"Most recent $prefix rooms: ",
                              Helpers
                                .commaSeparatedTags[Vector, RoomMetadata](
                                  rooms.toVector.sortBy(-_.creationTime).take(10),
                                  { case roomMeta =>
                                    <.a(
                                      ^.href := "#",
                                      roomMeta.name,
                                      joinDebate.whenDefined(join =>
                                        ^.onClick --> join(isOfficial.value, roomMeta.name)
                                      )
                                    )
                                  }
                                )
                                .toVdomArray
                            )
                            .when(rooms.nonEmpty)
                        },
                        createDebateValidated
                          .swap
                          .toOption
                          .whenDefined(_.toList.flatten.toVdomArray)
                      ),
                      roundTypeConfig(
                        "Opening Rounds",
                        setup
                          .zoomStateL(DebateSetupSpec.rules.composeLens(DebateRules.fixedOpening)),
                        minItems = 0
                      ),
                      roundTypeConfig(
                        "Repeated Rounds",
                        setup.zoomStateL(
                          DebateSetupSpec.rules.composeLens(DebateRules.repeatingStructure)
                        ),
                        minItems = 1
                      ),
                      globalQuoteRestrictionConfig(
                        setup.zoomStateL(
                          DebateSetupSpec.rules.composeLens(DebateRules.globalQuoteRestriction)
                        )
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
                            choices =
                              None +: indexOpt.foldMap(_.toList.sortBy(_.swap)).map(Some(_)),
                            curChoice = articleIdOpt.map(id => id -> getTitle(id)),
                            setChoice =
                              choice =>
                                choice match {
                                  case None =>
                                    setup
                                      .zoomStateL(DebateSetupSpec.sourceMaterial)
                                      .setState(
                                        CustomSourceMaterialSpec("Title", "Custom source material.")
                                      )
                                  case Some((articleId, _)) =>
                                    setup
                                      .zoomStateL(DebateSetupSpec.sourceMaterial)
                                      .setState(QuALITYSourceMaterialSpec(articleId))
                                }
                          ),
                          setup.value.sourceMaterial match {
                            case CustomSourceMaterialSpec(_, _) =>
                              val customSourceMaterialSpec =
                                setup
                                  .zoomStateO(
                                    DebateSetupSpec
                                      .sourceMaterial
                                      .composePrism(SourceMaterialSpec.custom)
                                  )
                                  .get // will succeed bc of match case
                              ReactFragment(
                                // title
                                V.LiveTextField
                                  .String
                                  .modInput(input = c"form-control")(
                                    customSourceMaterialSpec
                                      .zoomStateL(CustomSourceMaterialSpec.title)
                                  ),
                                // article
                                V.LiveTextArea
                                  .String
                                  .mod(textarea = TagMod(c"form-control", ^.rows := 10))(
                                    customSourceMaterialSpec
                                      .zoomStateL(CustomSourceMaterialSpec.contents),
                                    placeholderOpt = Some("Paste source material here")
                                  )
                              )
                            case QuALITYSourceMaterialSpec(articleId) =>
                              ReactFragment(
                                <.input(
                                  c"form-control",
                                  ^.readOnly := true,
                                  ^.value    := getTitle(articleId)
                                ),
                                <.textarea(
                                  c"form-control",
                                  ^.rows     := 10,
                                  ^.readOnly := true,
                                  ^.value    := storyOpt.fold("(loading story contents)")(_.article)
                                )
                              )
                          }
                        )
                      ),
                      <.div(S.mainLabeledInputRow)(
                        <.div(S.inputLabel)("Question"),
                        <.div(S.inputRowContents)(
                          QuestionSelect.modFull(select = S.customSelect)(
                            choices =
                              None +: storyOpt.foldMap(_.questions.values.toList).map(Some(_)),
                            curChoice = qualityQuestionOpt.value,
                            setChoice = {
                              case None =>
                                qualityQuestionOpt.setState(None) >>
                                  setup.modState(
                                    DebateSetupSpec
                                      .question
                                      .set("")
                                      .andThen(DebateSetupSpec.answers.set(Vector("", "")))
                                  )
                              case Some(question) =>
                                import io.circe.syntax._
                                Callback(org.scalajs.dom.console.log(question.asJson.spaces2)) >>
                                  qualityQuestionOpt.setState(Some(question)) >>
                                  setup.modState(setup =>
                                    setup.copy(
                                      question = question.question,
                                      answers = question.options,
                                      correctAnswerIndex =
                                        question
                                          .annotations
                                          .fold(setup.correctAnswerIndex)(
                                            _.writerLabel - 1
                                          ) // writer labels are 1-indexed
                                    )
                                  )
                            }
                          ),
                          V.LiveTextField.String(setup.zoomStateL(DebateSetupSpec.question)),
                          <.div(S.row)(
                            <.div(S.inputLabel)("Judge:"),
                            ProfileOptSelect.mod(select = S.customSelect)(
                              choices = profiles,
                              choice = setup.zoomStateL(
                                DebateSetupSpec.roles.composeLens(Optics.at(Judge: DebateRole))
                              )
                            )
                          )
                        )
                      ),
                      <.div(S.mainLabeledInputRow)(
                        <.div(S.inputLabel)("Answers"),
                        <.div(S.inputRowContents)(
                          ListConfig.String(setup.zoomStateL(DebateSetupSpec.answers), 1) {
                            (remove, answer, index) =>
                              <.div(S.row)(
                                <.span(S.answerLabel)(remove, " ", s"${answerLetter(index)}. "),
                                <.div(S.col, S.grow)(
                                  <.div(S.row)(V.LiveTextField.String(answer)),
                                  <.div(S.row)(
                                    <.input(S.correctAnswerRadio)(
                                      ^.`type`  := "radio",
                                      ^.name    := "correctAnswerIndex",
                                      ^.value   := index,
                                      ^.checked := setup.value.correctAnswerIndex == index,
                                      ^.onChange -->
                                        setup
                                          .zoomStateL(DebateSetupSpec.correctAnswerIndex)
                                          .setState(index)
                                    ),
                                    <.span(S.inputRowItem)(
                                      <.span(S.correctAnswerLabel)("Correct"),
                                      S.hidden.when(setup.value.correctAnswerIndex != index)
                                    ),
                                    <.div(S.inputLabel)("Debater:"),
                                    ProfileOptSelect.mod(select = S.customSelect)(
                                      choices = profiles,
                                      choice = setup.zoomStateL(
                                        DebateSetupSpec
                                          .roles
                                          .composeLens(Optics.at(Debater(index): DebateRole))
                                      )
                                    )
                                  )
                                )
                              )
                          }
                        )
                      )
                    )
                  }

                }
            }
        }
      }
    }
  }

}
