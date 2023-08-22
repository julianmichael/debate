package debate
package view.lobby

import scala.collection.immutable.SortedSet
import scala.util.Failure
import scala.util.Success

import cats.data.NonEmptySet
import cats.implicits._

import io.circe.disjunctionCodecs._
import io.circe.generic.JsonCodec
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

import jjm.OrWrapped
import jjm.ui.CacheCallContent

import debate.quality.QuALITYStory
import debate.scheduler.DebateScheduler
import debate.service.QuALITYStoryMetadata
import debate.util.Checkbox2
import debate.util.Local
import debate.util.NumberField2
import debate.util.ProbabilitySliders2
import debate.util.Slider2
import debate.util.SparseDistribution
import japgolly.scalajs.react.extra.StateSnapshot

object DebateSchedulingPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

//   val QuALITYStoryFetch = new CacheCallContent[String, QuALITYStory]
//   val QuALITYStorySelect =
//     new V.Select[(String, String)](
//       show =
//         choice =>
//           choice match {
//             case (articleId, title) =>
//               s"$title ($articleId)"
//           }
//     )

  import App.ajaxService

  val IndexFetch = new CacheCallContent[Unit, Map[String, QuALITYStoryMetadata]]
  //   def getSourceMaterialIndex: F[Map[String, QuALITYStoryMetadata]]
  val StoryAndMatchesFetch = new CacheCallContent[String, (QuALITYStory, Set[String])]
  //   def getStoryAndMatches(articleId: String): F[(QuALITYStory, Set[String])]

  val StoryOptSelect =
    new V.OptionalSelect[QuALITYStoryMetadata](
      show =
        choice =>
          choice match {
            case storyMetadata =>
              s"${storyMetadata.title} (${storyMetadata.articleId})"
          },
      none = "Choose story..."
    )

//   val QuALITYStorySelect =
//     new V.Select[Option[(String, String)]](
//       show =
//         choice =>
//           choice match {
//             case None =>
//               "(custom)"
//             case Some((articleId, title)) =>
//               s"$title ($articleId)"
//           }
//     )

  @Lenses
  @JsonCodec
  case class SchedulingSpec(
    canJudge: Set[String],
    canDebate: Set[String],
    workloadDist: SparseDistribution[String],
    ruleDist: SparseDistribution[RuleConfig],
    numDebatesPerQuestion: Int,
    dontAssignNewReading: Boolean,
    enforceJudgingConstraints: Boolean,
    numUniqueDebatersConstraint: Option[Int],
    articleMetadataOpt: Option[QuALITYStoryMetadata],
    questionIds: Set[String]
  )
  object SchedulingSpec {
    def init(
      people: NonEmptySet[String],
      ruleConfigs: NonEmptySet[RuleConfig],
      numDebatesPerQuestion: Int,
      dontAssignNewReading: Boolean,
      enforceJudgingConstraints: Boolean,
      numUniqueDebatersConstraint: Option[Int]
    ) = SchedulingSpec(
      canJudge = people.toSortedSet,
      canDebate = people.toSortedSet,
      SparseDistribution.uniform(people),
      SparseDistribution.uniform(ruleConfigs),
      numDebatesPerQuestion,
      dontAssignNewReading,
      enforceJudgingConstraints,
      numUniqueDebatersConstraint,
      None,
      Set()
    )
  }

  def sourceFiltersConfig(sourceFilters: StateSnapshot[StoryAndQuestionFilters]) = {
    val colStyle = TagMod(c"col-md-2 mb-2")
    ReactFragment(
      <.div(
        c"row"
        // S.row
        // S.rowWithGap
      )(
        <.div(colStyle)(
          <.div("Story already debated"),
          Checkbox2(sourceFilters.zoomStateL(StoryAndQuestionFilters.debatedStories), Some("Yes")),
          Checkbox2(sourceFilters.zoomStateL(StoryAndQuestionFilters.nonDebatedStories), Some("No"))
        ),
        <.div(colStyle)(
          <.div("Story overlaps w/single-turn debate"),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.storiesWithOverlap),
            Some("Yes")
          ),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.storiesWithNoOverlap),
            Some("No")
          )
        ),
        <.div(colStyle)(
          <.div("Story from Gutenberg"),
          Checkbox2(sourceFilters.zoomStateL(StoryAndQuestionFilters.gutenberg), Some("Yes")),
          Checkbox2(sourceFilters.zoomStateL(StoryAndQuestionFilters.nonGutenberg), Some("No"))
        ),
        <.div(colStyle)(
          <.div("Question is from single-turn debate"),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.overlappingQuestions),
            Some("Yes")
          ),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.nonOverlappingQuestions),
            Some("No")
          )
        ),
        <.div(colStyle)(
          <.div("Question already has debates"),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.debatedQuestions),
            Some("Yes")
          ),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.nonDebatedQuestions),
            Some("No")
          )
        ),
        <.div(colStyle)(
          <.div("Question gold labels"),
          Checkbox2(sourceFilters.zoomStateL(StoryAndQuestionFilters.noLabels), Some("None")),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.writerLabelAgreesWithGoldLabel),
            Some("Writer = Gold")
          ),
          Checkbox2(
            sourceFilters.zoomStateL(StoryAndQuestionFilters.writerLabelDoesntAgreeWithGoldLabel),
            Some("Writer != Gold")
          )
        )
      ),
      <.div(c"row")(
        Slider2.mod(textSpan = c"col-sm-6", slider = c"col-sm-6")(
          sourceFilters.zoomStateL(StoryAndQuestionFilters.minUntimedAccuracyAgainstGold),
          0.0,
          1.0,
          Some("Min untimed accuracy"),
          numSigFigs = 2
        )
      ),
      <.div(c"row")(
        Slider2.mod(textSpan = c"col-sm-6", slider = c"col-sm-6")(
          sourceFilters.zoomStateL(StoryAndQuestionFilters.maxSpeedAccuracyAgainstGold),
          0.0,
          1.0,
          Some("Max speed accuracy"),
          numSigFigs = 2
        )
      ),
      <.div(c"row")(
        Slider2.mod(textSpan = c"col-sm-6", slider = c"col-sm-6")(
          sourceFilters.zoomStateL(StoryAndQuestionFilters.minAverageContextRequiredJudgment),
          1.0,
          4.0,
          Some("Minimum average 'context required' judgment"),
          numSigFigs = 2
        )
      ),
      <.p(
        "(1) a sentence or two, (2) long paragraph or two; (3) third of the passage; (4) most or all of the passage."
      )
    )
  }

  def apply(lobby: Lobby, createRooms: CreateRooms => Callback) =
    NonEmptySet.fromSet(SortedSet(lobby.profiles.keySet.toSeq: _*)) match {
      case None =>
        <.div(
          "There need to be active profiles in order to auto-schedule debates. See the Profiles tab."
        )
      case Some(people) =>
        NonEmptySet.fromSet(SortedSet(lobby.ruleConfigs.values.toSeq: _*)) match {
          case None =>
            <.div(
              "There need to be saved rulesets in order to auto-schedule debates. Set them in the Create Debates tab."
            )
          case Some(ruleConfigs) =>
            Local[SchedulingSpec].syncedWithLocalStorage(
              "scheduling-specification",
              SchedulingSpec.init(people, ruleConfigs, 1, false, true, None)
            ) { schedulingSpec =>
              Local[Option[Either[String, Vector[DebateSetup]]]]
                .syncedWithSessionStorage("candidate-schedule", None) { scheduleAttemptOpt =>
                  ReactFragment(
                    <.h3("Auto-Schedule Debates"),
                    <.div(c"card")(
                      <.div(c"card-body")(
                        <.h4(c"card-title")("Rules"),
                        ProbabilitySliders2[RuleConfig](
                          schedulingSpec.zoomStateL(SchedulingSpec.ruleDist)
                        ) { case ProbabilitySliders2.Context(item, _, prob, setProb) =>
                          <.div(S.row)(
                            <.span(S.genericProbSliderLabel)(item.name),
                            <.span(S.genericProbSliderProbLabel)(f"${prob * 100.0}%.0f%%"),
                            <.input(S.genericProbSlider)(
                              ^.`type` := "range",
                              ^.min    := 0.00,
                              ^.max    := 1.00,
                              ^.step   := 0.01,
                              ^.value  := prob,
                              ^.onChange ==>
                                ((e: ReactEventFromInput) => setProb(e.target.value.toDouble))
                            )
                          )
                        },
                        <.div(S.row, c"mt-1")(
                          NumberField2(
                            schedulingSpec.zoomStateL(SchedulingSpec.numDebatesPerQuestion),
                            Some("Number of debates per question")
                          )
                        )

                        // <.div(S.row, c"mt-1")(
                        //   Checkbox2(
                        //     schedulingSpec.zoomStateL(SchedulingSpec.dontAssignNewReading),
                        //     Some("Don't assign new reading")
                        //   )
                        // )

                      )
                    ),
                    <.div(c"card")(
                      <.div(c"card-body")(
                        <.h4(c"card-title")("Participants"),
                        <.div(
                          <.p(
                            <.span(^.color := "green", "can judge: "),
                            <.a(
                              "check all",
                              ^.href := "#",
                              ^.onClick -->
                                schedulingSpec
                                  .modState(SchedulingSpec.canJudge.set(people.toSortedSet))
                            ),
                            <.span(" "),
                            <.a(
                              "uncheck all",
                              ^.href := "#",
                              ^.onClick -->
                                schedulingSpec.modState(SchedulingSpec.canJudge.set(Set()))
                            )
                          ),
                          <.p(
                            <.span(^.color := "red", "can debate: "),
                            <.a(
                              "check all",
                              ^.href := "#",
                              ^.onClick -->
                                schedulingSpec
                                  .modState(SchedulingSpec.canDebate.set(people.toSortedSet))
                            ),
                            <.span(" "),
                            <.a(
                              "uncheck all",
                              ^.href := "#",
                              ^.onClick -->
                                schedulingSpec.modState(SchedulingSpec.canDebate.set(Set()))
                            )
                          )
                          // <.p(^.color := "red", "(can debate) ")
                        ),
                        ProbabilitySliders2[String](
                          schedulingSpec.zoomStateL(SchedulingSpec.workloadDist)
                        ) { case ProbabilitySliders2.Context(item, _, prob, setProb) =>
                          val canJudge = schedulingSpec
                            .zoomStateL(SchedulingSpec.canJudge)
                            .zoomStateL(Optics.at(item))
                          val canDebate = schedulingSpec
                            .zoomStateL(SchedulingSpec.canDebate)
                            .zoomStateL(Optics.at(item))
                          <.div(S.row)(
                            Checkbox2.mod(box = TagMod(c"form-check-input", S.judgeBox))(canJudge),
                            Checkbox2
                              .mod(box = TagMod(c"form-check-input", S.debaterBox))(canDebate),
                            <.span(S.genericProbSliderLabel)(item),
                            <.span(S.genericProbSliderProbLabel)(f"${prob * 100.0}%.0f%%"),
                            <.input(
                              S.genericProbSlider,
                              S.disabledSlider.when(!canJudge.value && !canDebate.value)
                            )( // TODO styling
                              ^.`type` := "range",
                              ^.min    := 0.00,
                              ^.max    := 1.00,
                              ^.step   := 0.01,
                              ^.value  := prob,
                              ^.onChange ==>
                                ((e: ReactEventFromInput) => setProb(e.target.value.toDouble))
                            )
                          )
                        },
                        <.div(S.row, c"mt-1")(
                          Checkbox2(
                            schedulingSpec.zoomStateL(SchedulingSpec.dontAssignNewReading),
                            Some("Don't assign new reading")
                          )
                        ),
                        <.div(S.row, c"mt-1")(
                          Checkbox2(
                            schedulingSpec.zoomStateL(SchedulingSpec.enforceJudgingConstraints),
                            Some("Enforce judging constraints")
                          )
                        ),
                        DebateRulesPanel.optionalIntInput(
                          schedulingSpec.zoomStateL(SchedulingSpec.numUniqueDebatersConstraint),
                          Some("Enforce a specific number of debaters in this assignment"),
                          1
                        )
                      )
                    ),
                    <.div(c"card")(
                      <.div(c"card-body")(
                        <.h4(c"card-title")("Questions"),
                        IndexFetch.make(
                          (),
                          _ => OrWrapped.wrapped(ajaxService.getSourceMaterialIndex)
                        ) {
                          case IndexFetch.Loading =>
                            <.div("Loading QuALITY index...")
                          case IndexFetch.Loaded(index) =>
                            Local[StoryAndQuestionFilters].syncedWithSessionStorage(
                              "source-filters",
                              StoryAndQuestionFilters()
                            ) { sourceFilters =>
                              ReactFragment(
                                ReactFragment(sourceFiltersConfig(sourceFilters)),
                                StoryOptSelect(
                                  choices = index
                                    .values
                                    .toSet
                                    .filter(sourceFilters.value.admitsStory),
                                  curChoice = schedulingSpec.value.articleMetadataOpt,
                                  setChoice =
                                    metadataOpt =>
                                      schedulingSpec.modState(
                                        SchedulingSpec
                                          .articleMetadataOpt
                                          .set(metadataOpt)
                                          .andThen(SchedulingSpec.questionIds.set(Set()))
                                      )
                                ),
                                schedulingSpec
                                  .value
                                  .articleMetadataOpt
                                  .map { curArticleMetadata =>
                                    StoryAndMatchesFetch.make(
                                      curArticleMetadata.articleId,
                                      articleId =>
                                        OrWrapped.wrapped(ajaxService.getStoryAndMatches(articleId))
                                    ) {
                                      case StoryAndMatchesFetch.Loading =>
                                        <.div("Loading QuALITY story information...")
                                      case StoryAndMatchesFetch
                                            .Loaded((story, matchingQuestionIds)) =>
                                        val visibleQuestions = story
                                          .questions
                                          .values
                                          .toVector
                                          .filter(q =>
                                            sourceFilters
                                              .value
                                              .admitsQuestion(
                                                q,
                                                matchingQuestionIds,
                                                lobby.officialRooms.map(_.question)
                                              )
                                          )
                                        ReactFragment(
                                          visibleQuestions.toVdomArray { question =>
                                            Checkbox2(
                                              schedulingSpec.zoomStateL(
                                                SchedulingSpec
                                                  .questionIds
                                                  .composeLens(Optics.at(question.questionUniqueId))
                                              ),
                                              Some(question.question)
                                            )
                                          }
                                        )
                                    }
                                  }
                              )
                            }
                        }
                      )
                    ),
                    <.button(c"btn btn-primary")(
                      "Auto-schedule",
                      schedulingSpec
                        .value
                        .articleMetadataOpt
                        .map(_.articleId)
                        .map(articleId =>
                          ^.onClick -->
                            ajaxService
                              .sampleSchedule(
                                schedulingSpec.value.canJudge,
                                schedulingSpec.value.canDebate,
                                schedulingSpec.value.workloadDist,
                                schedulingSpec.value.ruleDist,
                                articleId,
                                schedulingSpec.value.questionIds,
                                schedulingSpec.value.numDebatesPerQuestion,
                                schedulingSpec.value.dontAssignNewReading,
                                schedulingSpec.value.enforceJudgingConstraints,
                                schedulingSpec.value.numUniqueDebatersConstraint
                              )
                              .completeWith {
                                case Success(scheduleEither) =>
                                  scheduleAttemptOpt.setState(Some(scheduleEither))
                                case Failure(err) =>
                                  scheduleAttemptOpt.setState(Some(Left(err.getMessage())))
                              }
                        )
                        .whenDefined(x => x)
                    ),
                    scheduleAttemptOpt
                      .value
                      .map {
                        case Left(err) =>
                          <.div(c"alert alert-danger")(
                            <.h5(c"text-danger")("Error"),
                            <.p(c"text-danger")(err)
                          )
                        case Right(newSetups) =>
                          newSetups.toVdomArray(setup =>
                            <.div(c"p-3")(<.pre(DebateScheduler.renderAssignmentText(setup)))
                          )
                      },
                    scheduleAttemptOpt
                      .value
                      .flatMap(_.toOption)
                      .map { newSetups =>
                        <.button(c"btn btn-primary")(
                          "Commit",
                          ^.onClick --> createRooms(CreateRooms(true, newSetups))
                        )
                      }
                  )
                }
            }
        }
    }

}
