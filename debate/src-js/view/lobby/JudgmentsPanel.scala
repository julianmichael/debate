package debate
package view.lobby

import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import scalacss.ScalaCssReact._
import java.time.Instant
import java.time.ZoneId

object JudgmentsPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  case class Judgment(
    room: String,
    judge: String,
    debateType: String,
    judgeType: String,
    probabilityCorrect: Double,
    timestamp: Long
  )

  private val dateFormatter = java.time.format.DateTimeFormatter.ofPattern("LLLL d");
  def getDateStr(timestamp: Long): String = Instant
    .ofEpochMilli(timestamp)
    // TODO this should perhaps display it in the client's timezone
    .atZone(ZoneId.of("Z")) // see "time zones" on http://cquiroz.github.io/scala-java-time/
    .format(dateFormatter)
  // .toLocalTime.toString
  // var string    = zonedDateTime.format(formatter);

  def getDebateType(roles: Set[LiveDebateRole]): String =
    if (
      roles
        .toList
        .collect { case Debater(_) =>
          ()
        }
        .size == 1
    )
      "Single"
    else if (roles.contains(Judge))
      "Double"
    else
      "Double (no judge)"

  // TODO: style row based on whether story is complete
  def apply(lobby: Lobby, connectToRoom: String => Callback) = {
    val thMod = c"text-left"
    <.table(c"table table-striped")(
      <.thead(
        <.tr(
          <.th(thMod)("Room"),
          <.th(thMod)("Judge"),
          <.th(thMod)("Debate Type"),
          <.th(thMod)("Judge Type"),
          <.th(thMod)("Judgment"),
          <.th(thMod)("Date")
        )
      ),
      <.tbody(
        lobby
          .officialRooms
          .flatMap { room =>
            Option(room.status)
              .collect { case RoomStatus.Complete(result, offlineJudgingResults, _) =>
                val liveResult = result
                  .judgingInfo
                  .map(judging =>
                    Judgment(
                      room.name,
                      room.roleAssignments(Judge),
                      getDebateType(room.roleAssignments.keySet),
                      "Live",
                      judging.finalJudgement(judging.correctAnswerIndex),
                      result.timestamp
                    )
                  )

                val offlineResults =
                  offlineJudgingResults
                    .flatMap { case (judge, judgment) =>
                      judgment
                        .result
                        .map { case JudgeFeedback(dist, speech, _) =>
                          Judgment(
                            room.name,
                            judge,
                            getDebateType(room.roleAssignments.keySet),
                            "Offline",
                            dist(room.result.get.correctAnswerIndex),
                            speech.timestamp
                          )
                        }
                    }
                    .toSet
                offlineResults ++ liveResult
              }
          }
          .flatten
          .toVector
          .sortBy(-_.timestamp)
          .toVdomArray {
            case Judgment(name, judge, debateType, judgeType, probCorrect, timestamp) =>
              <.tr(
                <.td(<.a(name, ^.href := "#", ^.onClick --> connectToRoom(name))),
                <.td(judge),
                <.td(debateType),
                <.td(judgeType),
                <.td(
                  Utils.probabilityBar(
                    TagMod.empty,
                    Vector(
                      Utils.ProbabilityBarItem(probCorrect, S.correctBg),
                      Utils.ProbabilityBarItem(1.0 - probCorrect, S.incorrectBg)
                    )
                  )
                ),
                <.td(getDateStr(timestamp))
              )
          }
      )
    )
  }
}
