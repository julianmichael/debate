package debate
package view.lobby

import cats.implicits._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.Callback

object HardQuestionsPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  def apply(lobby: Lobby, connectToRoom: String => Callback) = {
    val thMod = c"text-left"
    <.table(c"table table-striped")(
      <.thead(
        <.tr(
          <.th(thMod)("Question"),
          <.th(thMod)("Story"),
          <.th(thMod)("Debates"),
          <.th(c"text-right")("Rating")
        )
      ),
      <.tbody(
        lobby
          .leaderboard
          .ratings
          .questionEase
          .sortBy(_._2)
          .toVdomArray { case (QuestionId(sourceId, question), ease) =>
            val debateLinks = lobby
              .officialRooms
              .filter(room => room.sourceMaterialId == sourceId && room.question == question)
              .toVector
              .map(_.name)
              .sorted
              .map(name => <.a(name, ^.href := "#", ^.onClick --> connectToRoom(name)))
            <.tr(
              <.td(question),
              <.td(sourceId.title),
              <.td(Utils.delimitedTags(debateLinks, identity[VdomTag]).toVdomArray),
              <.td(c"text-right")(f"$ease%.2f")
            )
          }
      )
    )
  }
}
