package debate

import cats.~>
import cats.data.NonEmptyChain
import cats.data.NonEmptySet
import cats.data.Validated
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
import cats.kernel.Order
import scala.util.Random

object FacilitatorPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Helpers.ClassSetInterpolator

  val RoundTypeList = ListConfig[DebateRoundType](
    // DebateRoundType.SequentialSpeechesRound(500, None)
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

  val IntSetLocal      = new LocalState[Set[Int]]
  val QuestionOptLocal = new LocalState2[Option[QuALITYQuestion]]
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
  ) =
// div = TagMod(c"ml-3 pl-3 my-auto")
    ReactFragment(
      Checkbox2(
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
      NumberField2.mod(input = TagMod(c"col form-control ml-2", ^.disabled := field.value.isEmpty))(
        field.zoomStateL(Iso[Option[Int], Int](_.getOrElse(defaultInt))(Some(_)).asLens)
      )
    )

  def getValidatedCreateDebateCb(
    lobby: Lobby,
    roomName: StateSnapshot[String],
    isOfficial: Boolean,
    setup: DebateSetupSpec,
    initDebate: CreateRoom => Callback,
    joinDebate: Option[(Boolean, String) => Callback]
  ): Validated[NonEmptyChain[Option[VdomTag]], Callback] = {
    import cats.implicits._
    // import cats.syntax.validated._
    val createDebateCb: Callback =
      initDebate(
        CreateRoom(isOfficial = isOfficial, roomName = roomName.value.trim, setupSpec = setup)
      ) >> roomName.setState("")

    def ensure(key: String, condition: Boolean, err: Option[VdomTag]) =
      condition
        .validNec[Option[VdomTag]]
        .ensure(NonEmptyChain(err.map(_(c"text-danger", ^.key := key))))(identity)

    ensure("room name is not blank", roomName.value.nonEmpty, None) *>
      ensure(
        "at least 2 answers",
        setup.answers.filter(_.nonEmpty).size > 1,
        Some(<.div("Debates must have at least 2 answers."))
      ) *>
      ensure(
        "all roles assigned",
        !isOfficial || setup.areAllRolesAssigned,
        Some(<.div("All roles must be assigned in official debates."))
      ) *>
      ensure(
        "room doesn't exist",
        if (isOfficial)
          !lobby.officialRooms.exists(_.name == roomName.value.trim)
        else
          !lobby.practiceRooms.exists(_.name == roomName.value.trim),
        Some {
          val prefix =
            if (isOfficial)
              "official"
            else
              "practice"
          <.div(
            s"The $prefix room ",
            <.a(
              roomName.value.trim,
              ^.href := "#",
              joinDebate.whenDefined(cb => ^.onClick --> cb(isOfficial, roomName.value))
            ),
            " already exists."
          )
        }
      ) *>
      ensure(
        "correct answer index",
        0 <= setup.correctAnswerIndex && setup.correctAnswerIndex < setup.answers.size,
        Some(
          <.div(
            "Error: correct answer index is out of bounds. ",
            " (This is a bug and shouldn't happen — please report. But to work around it now, ",
            " just click the 'correct' radio button for one of the answers.)"
          )
        )
      ) *>
      ensure(
        "no dangling assignments",
        setup
          .roles
          .keySet
          .collect {
            case Debater(i) if i >= setup.answers.size =>
              i
          }
          .isEmpty,
        Some(
          <.div(
            "Error: there are dangling debater assignments for nonexistent answers.",
            " (This is a bug and shouldn't happen — please report. But to work around it now, ",
            " add answers until you find a debater assigned to the new answer then remove them.)"
          )
        )
      ) *> createDebateCb.validNec[Option[VdomTag]]
  }

  def headerBar(
    lobby: Lobby,
    setup: DebateSetupSpec,
    joinDebate: Option[(Boolean, String) => Callback],
    initDebate: CreateRoom => Callback,
    isOfficial: StateSnapshot[Boolean],
    roomName: StateSnapshot[String]
  ) = {
    val createDebateValidated: Validated[NonEmptyChain[Option[VdomTag]], Callback] =
      getValidatedCreateDebateCb(lobby, roomName, isOfficial.value, setup, initDebate, joinDebate)

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
              .delimitedTags[Vector, RoomMetadata](
                rooms.toVector.sortBy(-_.creationTime).take(10),
                { case roomMeta =>
                  <.a(
                    ^.href := "#",
                    roomMeta.name,
                    joinDebate
                      .whenDefined(join => ^.onClick --> join(isOfficial.value, roomMeta.name))
                  )
                }
              )
              .toVdomArray
          )
          .when(rooms.nonEmpty)
      },
      createDebateValidated.swap.toOption.whenDefined(_.toList.flatten.toVdomArray)
    )
  }

  /** Config panel for setting a list of round types. */
  def roundTypeSelect(roundTypes: StateSnapshot[Vector[DebateRoundType]], minItems: Int) = {
    val defaultRoundType = DebateRoundType.SequentialSpeechesRound(500, None)
    <.div(S.inputRowContents)(
      RoundTypeList.nice(roundTypes, defaultRoundType, minItems) {
        case ListConfig.Context(roundType, _) =>
          val rowMod = TagMod(c"form-inline")
          RoundTypeConfig
            .mod(select = TagMod(S.listCardHeaderSelect), optionsDiv = c"card-body")(roundType)(
              "Simultaneous Speeches" ->
                SumConfigOption(
                  DebateRoundType.SimultaneousSpeechesRound(500, None),
                  DebateRoundType.simultaneousSpeechesRound
                ) { simulSpeeches =>
                  ReactFragment(
                    <.div(rowMod, c"mb-1")(
                      NumberField2(
                        simulSpeeches
                          .zoomStateL(DebateRoundType.SimultaneousSpeechesRound.charLimit),
                        Some("Character limit")
                      )
                    ),
                    <.div(rowMod)(
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
                    <.div(rowMod, c"mb-1")(
                      NumberField2(
                        seqSpeeches.zoomStateL(DebateRoundType.SequentialSpeechesRound.charLimit),
                        labelOpt = Some("Character limit")
                      )
                    ),
                    <.div(rowMod)(
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
                    <.div(rowMod, c"mb-1")(
                      NumberField2(
                        judgeFeedback.zoomStateL(DebateRoundType.JudgeFeedbackRound.charLimit),
                        labelOpt = Some("Character limit")
                      )
                    ),
                    <.div(rowMod)(
                      Checkbox2(
                        judgeFeedback.zoomStateL(DebateRoundType.JudgeFeedbackRound.reportBeliefs),
                        Some("Report probabilities")
                      )
                    )
                  )
                }
            )
      }
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
      buttonContent = "Create",
      isEnabled = createDebateOpt.nonEmpty,
      enter = createDebateOpt.combineAll
    ),
    <.div(S.row)(V.Checkbox(isOfficial, "Official debate"))
  )

  def roundTypeConfig(
    label: String,
    rounds: StateSnapshot[Vector[DebateRoundType]],
    minItems: Int
  ) = <.div(S.mainLabeledInputRow)(<.div(S.inputRowLabel)(label), roundTypeSelect(rounds, minItems))

  def scoringFunctionConfig(scoringFunction: StateSnapshot[ScoringFunction]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Judge Scoring Function"),
      <.div(S.inputRowContents)(
        ScoringFunctionConfig(scoringFunction)(
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
    )

  def globalQuoteRestrictionConfig(quoteRestriction: StateSnapshot[Option[Int]]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Global Evidence Restrictions"),
      <.div(S.inputRowContents)(
        <.div(c"form-inline")(
          optionalIntInput(quoteRestriction, Some("Quote character limit"), defaultGlobalQuoteLimit)
        )
      )
    )

  def sourceMaterialConfig(
    sourceMaterial: StateSnapshot[SourceMaterialSpec],
    indexOpt: Option[Map[String, String]],
    articleIdOpt: Option[String],
    qualityStoryOpt: Option[QuALITYStory]
  ) = {
    def getTitle(articleId: String): String = indexOpt
      .flatMap(_.get(articleId))
      .getOrElse("(loading title)")
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Source Material"),
      <.div(S.inputRowContents)(
        QuALITYStorySelect.modFull(select = S.customSelect)(
          choices = None +: indexOpt.foldMap(_.toList.sortBy(_.swap)).map(Some(_)),
          curChoice = articleIdOpt.map(id => id -> getTitle(id)),
          setChoice =
            choice =>
              choice match {
                case None =>
                  sourceMaterial
                    .setState(CustomSourceMaterialSpec("Title", "Custom source material."))
                case Some((articleId, _)) =>
                  sourceMaterial.setState(QuALITYSourceMaterialSpec(articleId))
              }
        ),
        sourceMaterial.value match {
          case CustomSourceMaterialSpec(_, _) =>
            val customSourceMaterialSpec =
              sourceMaterial
                .zoomStateO(SourceMaterialSpec.custom.asOptional)
                .get // will succeed bc of match case
            ReactFragment(
              // title
              V.LiveTextField
                .String
                .modInput(input = c"form-control")(
                  customSourceMaterialSpec.zoomStateL(CustomSourceMaterialSpec.title)
                ),
              // article
              V.LiveTextArea
                .String
                .mod(textarea = TagMod(c"form-control", ^.rows := 10))(
                  customSourceMaterialSpec.zoomStateL(CustomSourceMaterialSpec.contents),
                  placeholderOpt = Some("Paste source material here")
                )
            )
          case QuALITYSourceMaterialSpec(articleId) =>
            ReactFragment(
              <.input(c"form-control", ^.readOnly := true, ^.value := getTitle(articleId)),
              <.textarea(
                c"form-control",
                ^.rows     := 10,
                ^.readOnly := true,
                ^.value    := qualityStoryOpt.fold("(loading story contents)")(_.article)
              )
            )
        }
      )
    )
  }

  def questionConfig(
    profiles: Set[String],
    setup: StateSnapshot[DebateSetupSpec],
    qualityStoryOpt: Option[QuALITYStory],
    qualityQuestionOpt: StateSnapshot[Option[QuALITYQuestion]]
  ) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Question"),
      <.div(S.inputRowContents)(
        QuestionSelect.modFull(select = S.customSelect)(
          choices = None +: qualityStoryOpt.foldMap(_.questions.values.toList).map(Some(_)),
          curChoice = qualityQuestionOpt.value,
          setChoice = {
            case None =>
              val init = DebateSetupSpec.init
              qualityQuestionOpt.setState(None) >>
                setup.modState(
                  _.copy(
                    question = init.question,
                    answers = init.answers,
                    correctAnswerIndex = init.correctAnswerIndex
                  )
                )
            case Some(question) =>
              import io.circe.syntax._
              Callback(org.scalajs.dom.console.log(question.asJson.spaces2)) >>
                setup.modState(setup =>
                  setup.copy(
                    question = question.question,
                    answers = question.options,
                    correctAnswerIndex =
                      question.annotations.fold(0)(_.goldLabel - 1) // writer labels are 1-indexed
                  )
                ) >> qualityQuestionOpt.setState(Some(question))
          }
        ),
        V.LiveTextField.String(setup.zoomStateL(DebateSetupSpec.question)),
        <.div(S.row)(
          <.div(S.inputLabel)("Judge:"),
          ProfileOptSelect.mod(select = S.customSelect)(
            choices = profiles,
            choice = setup
              .zoomStateL(DebateSetupSpec.roles.composeLens(Optics.at(Judge: DebateRole)))
          )
        )
      )
    )

  def answersConfig(
    lobby: Lobby,
    setup: StateSnapshot[DebateSetupSpec],
    qualityQuestionOpt: Option[QuALITYQuestion]
  ) = {
    IntSetLocal.make(qualityQuestionOpt.foldMap(_.bestDistractors.map(_ - 1))) { bestDistractors =>
      <.div(S.mainLabeledInputRow)(
        <.div(S.inputRowLabel)("Answers"),
        <.div(S.inputRowContents) {

          val rearrangeAnswers =
            (f: Vector ~> Vector) => {
              val recombination = f(setup.value.answers.indices.toVector)
              val newIndices = recombination
                .zipWithIndex
                .foldMap { case (oldIndex, newIndex) =>
                  Map(oldIndex -> NonEmptySet.of(newIndex))
                }
                .map { case (oldIndex, newIndices) =>
                  val newIndex = newIndices.minimum(Order.by((i: Int) => math.abs(oldIndex - i)))

                  oldIndex -> newIndex
                }
              setup.modState(
                _.copy(
                  answers = f(setup.value.answers),
                  correctAnswerIndex = newIndices
                    .get(setup.value.correctAnswerIndex)
                    .getOrElse(
                      Helpers.clamp(0, setup.value.correctAnswerIndex, recombination.size - 1)
                    ),
                  roles = setup
                    .value
                    .roles
                    .flatMap {
                      case (Debater(i) -> name) =>
                        newIndices.get(i).map(j => Debater(j) -> name)
                      case x =>
                        Some(x)
                    }
                )
              ) >> bestDistractors.modState(_.flatMap(newIndices.get))

            }

          ReactFragment(
            qualityQuestionOpt
              .flatMap(q => q.annotations.map(q -> _))
              .filter { case (_, a) =>
                a.goldLabel != a.writerLabel
              }
              .map { case (q, a) =>
                if (setup.value.correctAnswer == q.options(a.goldLabel - 1))
                  Some("gold label")
                else if (setup.value.correctAnswer == q.options(a.writerLabel - 1))
                  Some("writer label")
                else
                  None
              }
              .map(labelNameOpt =>
                <.div(c"alert alert-danger mb-1")(
                  "Gold and writer labels disagree",
                  labelNameOpt match {
                    case None =>
                      " "
                    case Some(labelName) =>
                      <.span(". The ", <.strong(labelName), " is currently marked as correct ")
                  },
                  " (see console for more)."
                )
              ),
            qualityQuestionOpt
              .flatMap(q => q.annotations.map(q -> _))
              .filter { case (q, a) =>
                !Set(q.options(a.goldLabel - 1), q.options(a.writerLabel - 1))
                  .contains(setup.value.correctAnswer)
              }
              .map(_ =>
                <.div(c"alert alert-warning mb-1")(
                  "The text of the correct answer does not match the original gold/writer answer",
                  " (see console for more)."
                )
              ),
            qualityQuestionOpt
              .filter(_.annotations.isEmpty)
              .map(_ =>
                <.div(c"alert alert-warning mb-1")(
                  "Annotations — gold answer, distractors, etc. — are not provided for this question",
                  " (see console for more)."
                )
              ),
            ListConfig
              .String
              .nice(
                items = setup.zoomStateL(DebateSetupSpec.answers),
                defaultItem = "",
                minItems = 1,
                rearrange = rearrangeAnswers
              ) { case ListConfig.Context(answer, index) =>
                <.div(c"card-body", S.row)(
                  <.span(c"col-form-label mr-2")(s"${answerLetter(index)}. "),
                  <.div(S.col, S.grow)(
                    V.LiveTextField.String(answer),
                    <.div(S.row, c"mt-1")(
                      <.input(S.correctAnswerRadio)(
                        ^.`type`  := "radio",
                        ^.name    := "correctAnswerIndex",
                        ^.value   := index,
                        ^.checked := setup.value.correctAnswerIndex == index,
                        ^.onChange -->
                          setup.zoomStateL(DebateSetupSpec.correctAnswerIndex).setState(index)
                      ),
                      <.span(c"mr-2", S.inputRowItem)(
                        <.span(S.correctAnswerLabel)("Correct"),
                        S.hidden.when(setup.value.correctAnswerIndex != index)
                      ),
                      <.div(S.inputLabel)("Debater:"),
                      ProfileOptSelect.mod(select = S.customSelect)(
                        choices = lobby.trackedDebaters,
                        choice = setup.zoomStateL(
                          DebateSetupSpec.roles.composeLens(Optics.at(Debater(index): DebateRole))
                        )
                      )
                    ),
                    <.div(c"mt-1 text-danger")("Best distractor")
                      .when(bestDistractors.value.contains(index))
                  )
                )
              },
            <.div(
              <.button(c"btn btn-outline-secondary mr-1")(
                "Shuffle answers",
                ^.onClick -->
                  Callback.lazily {
                    val seed = Random.nextInt()
                    rearrangeAnswers(λ[Vector ~> Vector]((new Random(seed)).shuffle(_)))
                  }
              ),
              <.button(c"btn btn-outline-secondary mr-1")(
                "Shuffle Debaters",
                ^.onClick -->
                  Callback.lazily {
                    val permutation = scala
                      .util
                      .Random
                      .shuffle(0.until(setup.value.answers.size).toVector)
                    setup
                      .zoomStateL(DebateSetupSpec.roles)
                      .modState(
                        _.map {
                          case (Debater(i) -> name) =>
                            Debater(permutation(i)) -> name
                          case x =>
                            x
                        }
                      )
                  }
              )
            )
          )
        }
      )
    }
  }

  /** Config panel for facilitator to set the rules of the debate. */
  def apply(
    lobby: Lobby,
    qualityService: QuALITYService[AsyncCallback],
    joinDebate: Option[(Boolean, String) => Callback],
    initDebate: CreateRoom => Callback
  ) =
    DebateSetupSpecLocal.syncedWithSessionStorage("debate-setup", DebateSetupSpec.init) { setup =>
      StringLocal.syncedWithSessionStorage("debate-setup-room-name", "") { roomName =>
        BoolLocal.syncedWithSessionStorage("debate-setup-is-official", true) { isOfficial =>
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
                val articleIdOpt = SourceMaterialSpec
                  .quality
                  .getOption(setup.value.sourceMaterial)
                  .map(_.articleId)
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
                  val qualityStoryOpt = storyOptFetch.toOption.flatten
                  QuestionOptLocal.syncedWithSessionStorage("selected-question", None) {
                    qualityQuestionOpt =>
                      <.div(S.facilitatorColumn, S.spaceySubcontainer)(
                        headerBar(lobby, setup.value, joinDebate, initDebate, isOfficial, roomName),
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
                        sourceMaterialConfig(
                          setup.zoomStateL(DebateSetupSpec.sourceMaterial),
                          indexOpt,
                          articleIdOpt,
                          qualityStoryOpt
                        ),
                        questionConfig(
                          lobby.trackedDebaters,
                          setup,
                          qualityStoryOpt,
                          qualityQuestionOpt
                        ),
                        answersConfig(lobby, setup, qualityQuestionOpt.value)
                      )
                  }

                }
            }
        }
      }
    }

}
