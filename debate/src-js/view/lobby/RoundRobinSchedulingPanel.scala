package debate
package view.lobby

import scala.collection.immutable.SortedSet
import scala.util.Failure
import scala.util.Success

import cats.data.NonEmptySet
import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

import jjm.Duad
// import jjm.OrWrapped
import jjm.ui.CacheCallContent
// import jjm.implicits._

import debate.quality.QuALITYStory
import debate.service.QuALITYStoryMetadata
import debate.util.Local
import debate.util.Checkbox2

import io.circe.generic.JsonCodec

object RoundRobinSchedulingPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

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

  val AIOptSelect = new V.OptionalSelect[Profile.AI](show = _.name, none = "Choose AI...")

  @Lenses
  @JsonCodec
  case class SchedulingSpec(
    canJudge: Set[Profile.Human] = Set(),
    canDebate: Set[Profile.Human] = Set(),
    debaterPairs: Set[Duad[Profile.Human]] = Set(),
    aiDebater: Option[Profile.AI] = None,
    sourceFilters: StoryAndQuestionFilters = StoryAndQuestionFilters()
  )

  def apply(lobby: Lobby) =
    NonEmptySet.fromSet(SortedSet(lobby.profiles.values.toSeq: _*)) match {
      case None =>
        <.div(
          "There need to be active profiles in order to auto-schedule debates. See the Profiles tab."
        )
      case Some(profiles) =>
        Local[SchedulingSpec]
          .syncedWithLocalStorage("round-robin-scheduling-state", SchedulingSpec()) {
            schedulingSpec =>
              val allPairs = {
                def loop(remainingDebaters: List[Profile.Human]): Set[Duad[Profile.Human]] =
                  remainingDebaters match {
                    case Nil =>
                      Set()
                    case debater :: rest =>
                      rest.map(Duad(debater, _)).toSet ++ loop(rest)
                  }
                loop(schedulingSpec.value.canDebate.toList)
              }

              Local[StoryAndQuestionFilters]
                .syncedWithSessionStorage("source-filters", StoryAndQuestionFilters()) {
                  sourceFilters =>
                    ReactFragment(
                      <.h3("Auto-Schedule Round Robin Debates"),
                      <.div(c"card")(
                        <.div(c"card-body")(
                          <.h4(c"card-title")("Participants"),
                          <.p(
                            <.span(^.color := "green", "(can judge) "),
                            <.span(^.color := "red", "(can debate) ")
                          ),
                          profiles
                            .value
                            .toSortedSet
                            .flatMap(Profile.human.getOption)
                            .toVdomArray { person =>
                              val canJudge = schedulingSpec
                                .zoomStateL(SchedulingSpec.canJudge)
                                .zoomStateL(Optics.at(person))
                              val canDebate = schedulingSpec
                                .zoomStateL(SchedulingSpec.canDebate)
                                .zoomStateL(Optics.at(person))
                              def maybeRemoveDebater(shouldNotRemove: Boolean) =
                                if (shouldNotRemove)
                                  Callback.empty
                                else
                                  schedulingSpec
                                    .zoomStateL(SchedulingSpec.debaterPairs)
                                    .modState(_.filterNot(_.contains(person)))

                              <.div(S.row, ^.key := s"config-${person.name}")(
                                Checkbox2.mod(box = TagMod(c"form-check-input", S.judgeBox))(
                                  canJudge,
                                  didUpdate = maybeRemoveDebater
                                ),
                                Checkbox2.mod(box = TagMod(c"form-check-input", S.debaterBox))(
                                  canDebate,
                                  didUpdate = maybeRemoveDebater
                                ),
                                <.span(^.width := "14rem")(person.name)
                              )
                            }
                        )
                      ),
                      <.div(c"card")(
                        <.div(c"card-body")(
                          <.h4(c"card-title")("AI Debater"),
                          AIOptSelect(
                            profiles.toSortedSet.flatMap(Profile.ai.getOption),
                            schedulingSpec.zoomStateL(SchedulingSpec.aiDebater)
                          )
                        )
                      ),
                      <.div(c"card")(
                        <.div(c"card-body")(
                          <.h4(c"card-title")("Debater Pairs"),
                          allPairs.toVdomArray { pair =>
                            val isIncluded = schedulingSpec
                              .zoomStateL(SchedulingSpec.debaterPairs)
                              .zoomStateL(Optics.at(pair))

                            <.div(S.row, ^.key := s"config-${pair.map(_.name)}")(
                              Checkbox2
                                .mod(box = TagMod(c"form-check-input", S.judgeBox))(isIncluded),
                              <.span(^.width := "14rem")(pair.min.name),
                              <.span(^.width := "14rem")(pair.max.name)
                            )
                          }
                        )
                      ),
                      <.div(c"card")(
                        <.div(c"card-body")(
                          <.h4(c"card-title")("Questions"),
                          DebateSchedulingPanel.sourceFiltersConfig(sourceFilters)
                        )
                      ),
                      <.button(c"btn btn-primary")(
                        "Create debates",
                        schedulingSpec
                          .value
                          .aiDebater
                          .whenDefined(aiDebater =>
                            ^.onClick -->
                              ajaxService
                                .scheduleRoundRobin(
                                  sourceFilters.value,
                                  schedulingSpec.value.debaterPairs,
                                  schedulingSpec.value.canJudge,
                                  aiDebater
                                )
                                .completeWith {
                                  case Success(scheduleEither) =>
                                    Callback(println(scheduleEither))
                                  case Failure(err) =>
                                    Callback(System.err.println(err.getMessage()))
                                }
                          )
                      )
                    )
                }
          }
    }

}
