package debate
package view.lobby

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import japgolly.scalajs.react.Callback

object AnalyticsPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  def apply(userName: String, lobby: Lobby, connectToRoom: String => Callback) =
    TabNav("admin-tab", initialTabIndex = 1)(
      "Graphs" -> TabNav.tab(<.div(c"card-body", S.spaceySubcontainer)(GraphsPanel())),
      "Feedback" ->
        TabNav.tab(<.div(c"card-body", S.spaceySubcontainer)(OpenEndedFeedbackPanel(userName))),
      "Stories" ->
        TabNav.tab(<.div(c"card-body", S.spaceySubcontainer)(StoriesAnalyticsPanel(lobby))),
      "Questions" ->
        TabNav
          .tab(<.div(c"card-body", S.spaceySubcontainer)(HardQuestionsPanel(lobby, connectToRoom))),
      "Judgments" ->
        TabNav.tab(<.div(c"card-body", S.spaceySubcontainer)(JudgmentsPanel(lobby, connectToRoom)))
    )
}
