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

import debate.quality.QuALITYQuestion
import debate.quality.QuALITYStory
import debate.scheduler.DebateScheduler
import debate.service.QuALITYStoryMetadata
import debate.util.Checkbox2
import debate.util.Local
import debate.util.NumberField2
import debate.util.ProbabilitySliders2
import debate.util.SparseDistribution

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
    workloadDist: SparseDistribution[String],
    ruleDist: SparseDistribution[RuleConfig],
    numDebatesPerQuestion: Int,
    articleMetadataOpt: Option[QuALITYStoryMetadata],
    questionIds: Set[String]
  )
  object SchedulingSpec {
    def init(
      people: NonEmptySet[String],
      ruleConfigs: NonEmptySet[RuleConfig],
      numDebatesPerQuestion: Int
    ) = SchedulingSpec(
      SparseDistribution.uniform(people),
      SparseDistribution.uniform(ruleConfigs),
      numDebatesPerQuestion,
      None,
      Set()
    )
  }

  @Lenses
  @JsonCodec
  case class SourceFilters(
    gutenberg: Boolean = true,
    nonGutenberg: Boolean = false,
    debatedStories: Boolean = false,
    nonDebatedStories: Boolean = true,
    storiesWithOverlap: Boolean = true,
    storiesWithNoOverlap: Boolean = false,
    overlappingQuestions: Boolean = true,
    nonOverlappingQuestions: Boolean = false,
    noLabels: Boolean = false,
    writerLabelAgreesWithGoldLabel: Boolean = true,
    writerLabelDoesntAgreeWithGoldLabel: Boolean = false,
    minUntimedAccuracyAgainstGold: Double = 1.0,
    maxSpeedAccuracyAgainstGold: Double = 0.5
  ) {
    def admitsStory(metadata: QuALITYStoryMetadata): Boolean = {
      val overlap =
        if (metadata.numSingleTurnDebateMatches > 0)
          storiesWithOverlap
        else
          storiesWithNoOverlap
      val debated =
        if (metadata.hasBeenDebated)
          debatedStories
        else
          nonDebatedStories
      val source =
        if (metadata.source == "Gutenberg")
          gutenberg
        else
          nonGutenberg
      overlap && debated && source
    }

    def admitsQuestion(question: QuALITYQuestion, matches: Set[String]): Boolean = {
      if (matches.contains(question.questionUniqueId))
        overlappingQuestions
      else
        nonOverlappingQuestions
      question.annotations match {
        case None =>
          noLabels
        case Some(annotations) =>
          val labelAgr =
            if (annotations.goldLabel == annotations.writerLabel)
              writerLabelAgreesWithGoldLabel
            else
              writerLabelDoesntAgreeWithGoldLabel

          labelAgr && annotations.untimedAccuracyAgainstGold >= minUntimedAccuracyAgainstGold &&
          annotations.speedAccuracyAgainstGold <= maxSpeedAccuracyAgainstGold
      }
    }

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
              SchedulingSpec.init(people, ruleConfigs, 2)
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
                          NumberField2.apply(
                            schedulingSpec.zoomStateL(SchedulingSpec.numDebatesPerQuestion),
                            Some("Number of debates per question")
                          )
                        )
                      )
                    ),
                    <.div(c"card")(
                      <.div(c"card-body")(
                        <.h4(c"card-title")("Participants"),
                        ProbabilitySliders2[String](
                          schedulingSpec.zoomStateL(SchedulingSpec.workloadDist)
                        ) { case ProbabilitySliders2.Context(item, _, prob, setProb) =>
                          <.div(S.row)(
                            <.span(S.genericProbSliderLabel)(item),
                            <.span(S.genericProbSliderProbLabel)(f"${prob * 100.0}%.0f%%"),
                            <.input(S.genericProbSlider)( // TODO styling
                              ^.`type` := "range",
                              ^.min    := 0.00,
                              ^.max    := 1.00,
                              ^.step   := 0.01,
                              ^.value  := prob,
                              ^.onChange ==>
                                ((e: ReactEventFromInput) => setProb(e.target.value.toDouble))
                            )
                          )
                        }
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
                            Local[SourceFilters].syncedWithSessionStorage(
                              "source-filters",
                              SourceFilters()
                            ) { sourceFilters =>
                              ReactFragment(
                                <.div(S.rowWithGap)(
                                  <.div(S.col)(
                                    <.div("Story already debated"),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.debatedStories),
                                      Some("Yes")
                                    ),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.nonDebatedStories),
                                      Some("No")
                                    )
                                  ),
                                  <.div(S.col)(
                                    <.div("Story overlaps w/single-turn debate"),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.storiesWithOverlap),
                                      Some("Yes")
                                    ),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.storiesWithNoOverlap),
                                      Some("No")
                                    )
                                  ),
                                  <.div(S.col)(
                                    <.div("Story from Gutenberg"),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.gutenberg),
                                      Some("Yes")
                                    ),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.nonGutenberg),
                                      Some("No")
                                    )
                                  ),
                                  <.div(S.col)(
                                    <.div("Question is from single-turn debate"),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.overlappingQuestions),
                                      Some("Yes")
                                    ),
                                    Checkbox2(
                                      sourceFilters
                                        .zoomStateL(SourceFilters.nonOverlappingQuestions),
                                      Some("No")
                                    )
                                  ),
                                  <.div(S.col)(
                                    <.div("Question gold labels"),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(SourceFilters.noLabels),
                                      Some("None")
                                    ),
                                    Checkbox2(
                                      sourceFilters
                                        .zoomStateL(SourceFilters.writerLabelAgreesWithGoldLabel),
                                      Some("Writer = Gold")
                                    ),
                                    Checkbox2(
                                      sourceFilters.zoomStateL(
                                        SourceFilters.writerLabelDoesntAgreeWithGoldLabel
                                      ),
                                      Some("Writer != Gold")
                                    )
                                  )
                                ),
                                <.div(
                                  V.Slider(
                                    sourceFilters
                                      .zoomStateL(SourceFilters.minUntimedAccuracyAgainstGold),
                                    0.0,
                                    1.0,
                                    Some("Min untimed accuracy")
                                  )
                                ),
                                <.div(
                                  V.Slider(
                                    sourceFilters
                                      .zoomStateL(SourceFilters.maxSpeedAccuracyAgainstGold),
                                    0.0,
                                    1.0,
                                    Some("Max speed accuracy")
                                  )
                                ),
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
                                              .admitsQuestion(q, matchingQuestionIds)
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
                                schedulingSpec.value.workloadDist,
                                schedulingSpec.value.ruleDist,
                                articleId,
                                schedulingSpec.value.questionIds,
                                schedulingSpec.value.numDebatesPerQuestion
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
