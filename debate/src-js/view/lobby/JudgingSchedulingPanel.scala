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
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

import debate.scheduler.DebateScheduler
import debate.util.Local
import debate.util.NumberField2
import debate.util.SetConfig

object JudgingSchedulingPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  import App.ajaxService

  @Lenses
  @JsonCodec
  case class SchedulingSpec(
    excludes: Set[String],
    maxNumJudgesForOnline: Int,
    maxNumJudgesForOffline: Int
  )
  object SchedulingSpec {
    def init = SchedulingSpec(Set.empty, 2, 2)
  }

  def apply(lobby: Lobby, scheduleOfflineJudges: ScheduleOfflineJudges => Callback) =
    NonEmptySet.fromSet(SortedSet(lobby.profiles.keySet.toSeq: _*)) match {
      case None =>
        <.div(
          "There need to be active profiles in order to auto-schedule debates. See the Profiles tab."
        )
      case Some(people) =>
        Local[SchedulingSpec]
          .syncedWithLocalStorage("scheduling-specification", SchedulingSpec.init) {
            schedulingSpec =>
              Local[Option[Either[String, DebateScheduler.OfflineJudgeSchedulingResult]]]
                .syncedWithSessionStorage("candidate-schedule", None) { scheduleAttemptOpt =>
                  ReactFragment(
                    <.h3("Auto-Schedule Offline Judging"),
                    <.div(c"card")(
                      <.div(c"card-body")(
                        <.h4(c"card-title")("Excludes"),
                        SetConfig
                          .String
                          .nice(
                            people.toSortedSet.toSet,
                            items = schedulingSpec.zoomStateL(SchedulingSpec.excludes),
                            minItems = 0
                          ) { case SetConfig.Context(person) =>
                            <.div(c"p-2", S.row)(<.span(person))
                          },
                        <.div(S.row, c"mt-1")(
                          NumberField2.apply(
                            schedulingSpec.zoomStateL(SchedulingSpec.maxNumJudgesForOnline),
                            Some("Maximum number of judges for online debates")
                          )
                        ),
                        <.div(S.row, c"mt-1")(
                          NumberField2.apply(
                            schedulingSpec.zoomStateL(SchedulingSpec.maxNumJudgesForOffline),
                            Some("Maximum number of judges for offline debates")
                          )
                        )
                      )
                    ),
                    <.button(c"btn btn-primary")(
                      "Auto-schedule",
                      ^.onClick -->
                        ajaxService
                          .sampleOfflineJudging(
                            schedulingSpec.value.excludes,
                            schedulingSpec.value.maxNumJudgesForOnline,
                            schedulingSpec.value.maxNumJudgesForOffline
                          )
                          .completeWith {
                            case Success(Right(schedule)) =>
                              scheduleAttemptOpt.setState(Some(Right(schedule)))
                            case Success(Left(message)) =>
                              scheduleAttemptOpt
                                .setState(Some(Left(s"""Could not sample a valid set of assignments.
                                                       |Error: $message""".stripMargin.trim)))
                            case Failure(err) =>
                              scheduleAttemptOpt.setState(Some(Left(err.getMessage())))
                          }
                    ),
                    scheduleAttemptOpt
                      .value
                      .map {
                        case Left(err) =>
                          <.div(c"alert alert-danger")(
                            <.h5(c"text-danger")("Error"),
                            <.p(c"text-danger")(err)
                          )
                        case Right(result) =>
                          <.div(
                            result
                              .debatesWithoutNewAssignments
                              .toNes
                              .map { roomNames =>
                                <.div(c"alert alert-warning")(
                                  <.h5(c"text-warning")(
                                    "Some rooms need more judges, but couldn't have any assigned"
                                  ),
                                  <.p(c"text-warning")(
                                    Utils.delimitedSpans(roomNames.toList).toVdomArray
                                  )
                                )
                              },
                            result
                              .newAssignments
                              .toVdomArray { case (roomName, userName) =>
                                <.div(s"$roomName: $userName")
                              }
                          )
                      },
                    scheduleAttemptOpt
                      .value
                      .flatMap(_.toOption)
                      .map { result =>
                        <.button(c"btn btn-primary")(
                          "Commit",
                          ^.onClick -->
                            scheduleOfflineJudges(
                              ScheduleOfflineJudges(true, result.newAssignments)
                            )
                        )
                      }
                  )
                }
          }
    }

}
