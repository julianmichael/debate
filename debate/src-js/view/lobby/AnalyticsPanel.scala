package debate
package view.lobby

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator

object AnalyticsPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  def apply(userName: String) =
    TabNav("admin-tab", initialTabIndex = 1)(
      "Graphs" -> TabNav.tab(<.div(c"card-body", S.spaceySubcontainer)(GraphsPanel())),
      "Open-Ended Feedback" ->
        TabNav.tab(<.div(c"card-body", S.spaceySubcontainer)(OpenEndedFeedbackPanel(userName)))
    )
}
