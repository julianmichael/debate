package debate
package view.lobby

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.quality.QuALITYService
import debate.util.Local

object LobbyPage {
  val S = Styles
  val V = new jjm.ui.View(S)

  case class Props(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback
  )

  val Component =
    ScalaComponent
      .builder[Props]("Lobby Page")
      .render_P { case Props(qualityService, lobby, sendToMainChannel, connect) =>
        val noProfileString = "(select profile)"

        def setChoice(userName: StateSnapshot[String])(name: String) = {
          val adjustedName =
            if (name == noProfileString)
              ""
            else
              name
          userName.setState(adjustedName)
        }

        def curChoice(userName: StateSnapshot[String]) =
          if (lobby.trackedDebaters.contains(userName.value)) {
            userName.value
          } else
            noProfileString

        def profileSelector(isAdmin: StateSnapshot[Boolean], userName: StateSnapshot[String]) =
          <.div(c"form-group row")(
            <.label(c"col-sm-2 col-form-label")("Profile:", ^.onClick --> isAdmin.modState(!_)),
            V.Select
              .String
              .modFull(TagMod(c"col-sm-10", S.customSelect))(
                choices = noProfileString +: lobby.trackedDebaters.toList.sorted,
                curChoice = curChoice(userName),
                setChoice = setChoice(userName)
              )
          )

        Local[Boolean].syncedWithSessionStorage(key = "is-admin", defaultValue = false) { isAdmin =>
          Local[String].syncedWithLocalStorage(key = "profile", defaultValue = "") { userName =>
            <.div(S.lobbyContainer, S.spaceyContainer)(
              profileSelector(isAdmin = isAdmin, userName = userName),
              <.div(c"card") {
                val myDebates = lobby
                  .officialRooms
                  .filter(_.roleAssignments.values.toSet.contains(userName.value))

                val numDebatesMyTurn =
                  myDebates
                    .filter { room =>
                      val myRoles = room.roleAssignments.filter(_._2 == userName.value).keySet
                      myRoles.intersect(room.currentSpeakers).nonEmpty
                    }
                    .size

                TabNav("main-tab", 0)(
                  "Debates" ->
                    TabNav.tabWithNotifications(numDebatesMyTurn)(
                      DebatesPanel(
                        isAdmin = isAdmin.value,
                        lobby = lobby,
                        userName = userName.value,
                        connect = connect,
                        sendToMainChannel = sendToMainChannel
                      )
                    ),
                  "Leaderboard" -> TabNav.tab(LeaderboardPanel(lobby)),
                  "Admin" ->
                    TabNav.tab(
                      AdminPanel.make(
                        lobby = lobby,
                        qualityService = qualityService,
                        userName = userName.value,
                        connect = connect,
                        sendToMainChannel = sendToMainChannel
                      )
                    )
                )
              }
            )
          }
        }
      }
      .componentDidMount(_ => Callback(dom.window.document.title = Utils.makePageTitle("Lobby")))
      .build

  def make(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback
  ) = Component(Props(qualityService, lobby, sendToMainChannel, connect))
}
