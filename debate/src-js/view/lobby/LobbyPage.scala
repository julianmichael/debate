package debate
package view.lobby

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._

import jjm.ui.Mounting

import debate.Utils.ClassSetInterpolator
import debate.quality.QuALITYService

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
    isAdmin: Boolean,
    logout: Callback,
    userName: String
  ) =
    Mounting.make(Callback(dom.window.document.title = Utils.makePageTitle("Lobby"))) {
      <.div(S.lobbyContainer, S.spaceyContainer)(
        headerRow(userName, logout),
        <.div(c"card") {
          val numDebatesMyTurn =
            lobby
              .officialRooms
              .filter(_.roleAssignments.values.toSet.contains(userName))
              .filter { room =>
                val myRoles = room.roleAssignments.filter(_._2 == userName).keySet
                myRoles.intersect(room.currentSpeakers).nonEmpty
              }
              .size

          val numDebatesReadyToJudge =
            lobby
              .officialRooms
              .filter(_.offlineJudgeAssignments.contains(userName))
              .flatMap(r => RoomStatus.complete.getOption(r.status))
              .filter(_.offlineJudgingResults.get(userName).forall(_.result.isEmpty))
              .size

          TabNav("main-tab", 0)(
            "Debates" ->
              TabNav.tabWithNotifications(numDebatesMyTurn + numDebatesReadyToJudge)(
                DebatesPanel(
                  isAdmin = isAdmin,
                  lobby = lobby,
                  userName = userName,
                  connect = connect,
                  sendToMainChannel = sendToMainChannel
                )
              ),
            "Leaderboard" ->
              TabNav.tab(
                LeaderboardPanel(
                  lobby,
                  refreshLeaderboard = sendToMainChannel(RefreshLeaderboard())
                )
              ),
            "Analytics" ->
              TabNav.tab(
                AnalyticsPanel(
                  userName,
                  lobby,
                  room =>
                    connect(
                      ConnectionSpec(isOfficial = true, roomName = room, participantName = userName)
                    )
                )
              ),
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
