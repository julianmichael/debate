package debate
package view.lobby

import scala.util.Random

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.Validated
import cats.implicits._
import cats.kernel.Order
import cats.~>

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import scalacss.ScalaCssReact._

import jjm.OrWrapped
import jjm.ui.CacheCallContent

import debate.quality._
import debate.util._

object DebateCreationPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

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
        isOfficial --> setup.areAllRolesAssigned,
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
      ) *>
      ensure(
        "judge concludes finite-length debates when present",
        (setup.rules.hasJudge && setup.rules.fixedClosing.nonEmpty) -->
          setup.rules.roundTypes.lastOption.exists(_.hasJudge),
        Some(
          <.div(
            "In fixed-length debates with a live judge, the final round must be a Judge Feedback round."
          )
        )
      ) *>
      ensure(
        "debate must be able to end",
        setup.rules.canEnd,
        Some(<.div("Debate must be able to end (via judge or max length)"))
      ) *>
      ensure(
        "no judge assigned in judgeless debate",
        !setup.rules.hasJudge --> !setup.roles.contains(Judge),
        Some(<.div("No judge may be assigned in a judgeless debate."))
      ) *>
      ensure(
        "offline judges assigned in official judgeless debate",
        (isOfficial && !setup.rules.hasJudge) --> setup.offlineJudges.nonEmpty,
        Some(<.div("Offline judges must be assigned in official debates with no live judge."))
      ) *>
      ensure(
        "offline judges are not also assigned to live roles",
        setup.offlineJudges.keySet.intersect(setup.roles.values.toSet).isEmpty,
        Some(<.div("Offline judges cannot also be assigned to live roles."))
      ) *>
      ensure(
        "no judge in debates which can end by agreement",
        setup
          .rules
          .roundTypeSet
          .existsAs { case DebateRoundType.NegotiateEndRound =>
            true
          } --> !setup.rules.hasJudge,
        Some(
          <.div(
            "A debate which can end by mutual agreement must be judgeless. ",
            "We currently don't have a way of ensuring that the judge reports their beliefs ",
            "after reading the entire debate, and then extracting that report. ",
            "This shouldn't be a problem as long as we only use ending by mutual agreement for ",
            "offline-judged debates."
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

    val infoMessages =
      List(
        Option(<.div("Debate has no live judge, but can be used for offline judging."))
          .filter(_ => !setup.rules.hasJudge)
      ).flatten

    <.div(S.spaceySubcontainer, S.stickyBanner)(
      roomSettings(isOfficial, roomName, createDebateValidated.toOption),
      <.div {
        val (prefix, rooms) =
          if (isOfficial.value)
            "official" -> lobby.officialRooms
          else
            "practice" -> lobby.practiceRooms

        Local[Int]
          .make(5) { numMostRecentToShow =>
            <.span(
              s"Most recent $prefix rooms: ",
              Utils
                .delimitedTags[Vector, RoomMetadata](
                  rooms.toVector.sortBy(-_.creationTime).take(numMostRecentToShow.value),
                  { case roomMeta =>
                    <.a(
                      ^.href := "#",
                      roomMeta.name,
                      joinDebate
                        .whenDefined(join => ^.onClick --> join(isOfficial.value, roomMeta.name))
                    )
                  }
                )
                .toVdomArray,
              " ",
              <.a(c"text-muted")(
                  ^.href := "#",
                  "(show more)",
                  ^.onClick --> numMostRecentToShow.modState(_ + 5)
                )
                .when(numMostRecentToShow.value < rooms.size)
            )
          }
          .when(rooms.nonEmpty)
      },
      NonEmptyList
        .fromList(infoMessages)
        .whenDefined(msgs => <.div(c"alert alert-info mt-1")(msgs.toList.toVdomArray)),
      createDebateValidated
        .swap
        .toOption
        .whenDefined(errorMessages =>
          NonEmptyList
            .fromList(errorMessages.toList.flatten)
            .whenDefined(msgs => <.div(c"alert alert-danger mt-1")(msgs.toList.toVdomArray))
        )
    )
  }

  def roomSettings(
    isOfficial: StateSnapshot[Boolean],
    roomName: StateSnapshot[String],
    createDebateOpt: Option[Callback]
  ) = ReactFragment(
    Utils.textInputWithEnterButton(
      field = roomName,
      placeholderOpt = Some("Room name"),
      buttonContent = "Create",
      isEnabled = createDebateOpt.nonEmpty,
      enter = createDebateOpt.combineAll
    ),
    <.div(S.row)(V.Checkbox(isOfficial, "Official debate"))
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
                  sourceMaterial.setState(CustomSourceMaterialSpec.default)
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
              .zoomStateL(DebateSetupSpec.roles.composeLens(Optics.at(Judge: LiveDebateRole)))
          )
        )
      )
    )

  def answersConfig(
    lobby: Lobby,
    setup: StateSnapshot[DebateSetupSpec],
    qualityQuestionOpt: Option[QuALITYQuestion]
  ) = {
    Local[Set[Int]].make(
      qualityQuestionOpt.foldMap(
        _.annotations.map(a => a.bestDistractors(a.goldLabel)).getOrElse(Set.empty[Int]).map(_ - 1)
      )
    ) { bestDistractors =>
      <.div(S.mainLabeledInputRow)(
        <.div(S.inputRowLabel)("Answers"),
        <.div(S.inputRowContents) {

          val rearrangeAnswers =
            (f: Vector ~> Vector) =>
              Callback.lazily {
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
                        Utils.clamp(0, setup.value.correctAnswerIndex, recombination.size - 1)
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
            ListConfig[String].nice(
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
                      choices = lobby.profiles.keySet,
                      choice = setup.zoomStateL(
                        DebateSetupSpec.roles.composeLens(Optics.at(Debater(index): LiveDebateRole))
                      )
                    )
                  ),
                  <.div(c"mt-1 text-danger")("Best distractor")
                    .when(bestDistractors.value.contains(index))
                )
              )
            },
            <.div(
              <.button(c"btn btn-outline-secondary mr-1 mt-1")(
                "Shuffle answers",
                ^.onClick -->
                  Callback.lazily {
                    val seed = Random.nextInt()
                    rearrangeAnswers(λ[Vector ~> Vector]((new Random(seed)).shuffle(_)))
                  }
              ),
              <.button(c"btn btn-outline-secondary mr-1 mt-1")(
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

  def offlineJudgesConfig(
    profiles: Set[String],
    judges: StateSnapshot[Map[String, Option[OfflineJudgingMode]]]
  ) = {
    val offlineJudges =
      judges.zoomState[Set[String]](_.keySet)(newKeys =>
        _ => newKeys.map(_ -> Some(OfflineJudgingMode.Timed)).toMap
      )
    SetConfig
      .String
      .nice(profiles, items = offlineJudges, minItems = 0) { case SetConfig.Context(judge) =>
        <.div(c"p-2", S.row)(<.span(judge))
      }
  }

  def offlineJudgesPanel(lobby: Lobby, setup: StateSnapshot[DebateSetupSpec]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Offline Judges"),
      <.div(S.inputRowContents) {
        offlineJudgesConfig(lobby.profiles.keySet, setup.zoomStateL(DebateSetupSpec.offlineJudges))
      }
    )

  /** Config panel for facilitator to set the rules of the debate. */
  def make(
    lobby: Lobby,
    qualityService: QuALITYService[AsyncCallback],
    joinDebate: Option[(Boolean, String) => Callback],
    initDebate: CreateRoom => Callback
  ) = Component(Props(lobby, qualityService, joinDebate, initDebate))

  case class Props(
    lobby: Lobby,
    qualityService: QuALITYService[AsyncCallback],
    joinDebate: Option[(Boolean, String) => Callback],
    initDebate: CreateRoom => Callback
  )

  val Component =
    ScalaComponent
      .builder[Props]("Debate Creation Panel")
      .render_P { case Props(lobby, qualityService, joinDebate, initDebate) =>
        Local[DebateSetupSpec].syncedWithSessionStorage("debate-setup", DebateSetupSpec.init) {
          setup =>
            Local[String].syncedWithSessionStorage("debate-setup-room-name", "") { roomName =>
              Local[Boolean].syncedWithSessionStorage("debate-setup-is-official", true) {
                isOfficial =>
                  QuALITYIndexFetch.make(
                    request = (),
                    sendRequest = _ => OrWrapped.wrapped(qualityService.getIndex)
                  ) { indexFetch =>
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
                      Local[Option[QuALITYQuestion]]
                        .syncedWithSessionStorage("selected-question", None) { qualityQuestionOpt =>
                          <.div(S.facilitatorColumn, S.spaceySubcontainer)(
                            headerBar(
                              lobby,
                              setup.value,
                              joinDebate,
                              initDebate,
                              isOfficial,
                              roomName
                            ),
                            DebateRulesPanel(setup.zoomStateL(DebateSetupSpec.rules)),
                            sourceMaterialConfig(
                              setup.zoomStateL(DebateSetupSpec.sourceMaterial),
                              indexOpt,
                              articleIdOpt,
                              qualityStoryOpt
                            ),
                            questionConfig(
                              lobby.profiles.keySet,
                              setup,
                              qualityStoryOpt,
                              qualityQuestionOpt
                            ),
                            answersConfig(lobby, setup, qualityQuestionOpt.value),
                            offlineJudgesPanel(lobby, setup)
                          )
                        }
                    }
                  }
              }
            }
        }
      }
      .build

}
