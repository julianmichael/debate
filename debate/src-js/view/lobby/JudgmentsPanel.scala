package debate
package view.lobby

import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback
import scalacss.ScalaCssReact._

object JudgmentsPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  case class Judgment(
    room: String,
    judge: String,
    judgingType: String,
    probabilityCorrect: Double,
    timestamp: Long
  )

  // TODO: style row based on whether story is complete
  def apply(lobby: Lobby, connectToRoom: String => Callback) = {
    val thMod = c"text-left"
    <.table(
      <.thead(
        <.tr(
          <.th(thMod)("Room"),
          <.th(thMod)("Judge"),
          <.th(thMod)("Type"),
          <.th(thMod)("Judgment"),
          <.th(thMod)("Timestamp")
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
          .toVdomArray { case Judgment(name, judge, judgingType, probCorrect, timestamp) =>
            <.tr(
              <.td(<.a(name, ^.href := "#", ^.onClick --> connectToRoom(name))),
              <.td(judge),
              <.td(judgingType),
              <.td(
                Utils.probabilityBar(
                  TagMod.empty,
                  Vector(
                    Utils.ProbabilityBarItem(probCorrect, S.correctBg),
                    Utils.ProbabilityBarItem(1.0 - probCorrect, S.incorrectBg)
                  )
                )
              ),
              <.td(c"text-right")(timestamp.toString)
            )
          }
      )
    )
  }
}
