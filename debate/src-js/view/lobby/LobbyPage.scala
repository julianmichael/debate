package debate
package view.lobby

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.quality.QuALITYService
import jjm.ui.Mounting

object LobbyPage {
  val S = Styles
  val V = new jjm.ui.View(S)

  def headerRow(userName: String, logout: Callback) =
    <.div(c"row", S.spaceySubcontainer, ^.alignItems.center)(
      <.button(c"btn btn-sm", S.simpleSelectable, ^.fontSize.small)(
        <.i(c"bi bi-arrow-left"),
        " Logout",
        ^.onClick --> logout
      ),
      <.div(<.strong("Name: "), userName)
    )

  def apply(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback,
    isAdmin: StateSnapshot[Boolean],
    logout: Callback,
    userName: String
  ) =
    Mounting.make(Callback(dom.window.document.title = Utils.makePageTitle("Lobby"))) {
      <.div(S.lobbyContainer, S.spaceyContainer)(
        headerRow(userName, logout),
        // App.profileSelector(lobby.trackedDebaters, isAdmin = isAdmin, profile = profile),
        <.div(c"card") {
          val myDebates = lobby
            .officialRooms
            .filter(_.roleAssignments.values.toSet.contains(userName))

          val numDebatesMyTurn =
            myDebates
              .filter { room =>
                val myRoles = room.roleAssignments.filter(_._2 == userName).keySet
                myRoles.intersect(room.currentSpeakers).nonEmpty
              }
              .size

          TabNav("main-tab", 0)(
            "Debates" ->
              TabNav.tabWithNotifications(numDebatesMyTurn)(
                DebatesPanel(
                  isAdmin = isAdmin.value,
                  lobby = lobby,
                  userName = userName,
                  connect = connect,
                  sendToMainChannel = sendToMainChannel
                )
              ),
            "Leaderboard" -> TabNav.tab(LeaderboardPanel(lobby)),
            "Admin" ->
              TabNav.tab(
                AdminPanel(
                  lobby = lobby,
                  qualityService = qualityService,
                  userName = userName,
                  connect = connect,
                  sendToMainChannel = sendToMainChannel
                )
              )
          )
        }
      )
    }

}
