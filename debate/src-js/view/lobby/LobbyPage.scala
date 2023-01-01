package debate
package view.lobby

import cats.implicits._

import io.circe.generic.JsonCodec
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.quality.QuALITYService
import debate.util.LocalState2

object LobbyPage {
  val S = Styles
  val V = new jjm.ui.View(S)

  @JsonCodec
  sealed trait MainLobbyTab extends Product with Serializable {
    import MainLobbyTab._
    override def toString =
      this match {
        case Debates =>
          "Debates"
        case Leaderboard =>
          "Leaderboard"
        case Admin =>
          "Admin"
      }
  }
  object MainLobbyTab {
    case object Debates     extends MainLobbyTab
    case object Leaderboard extends MainLobbyTab
    case object Admin       extends MainLobbyTab

    def all = Vector(Debates, Leaderboard, Admin)
  }

  val LocalBool   = new LocalState2[Boolean]
  val LocalString = new LocalState2[String]

  val LocalMainTab = new LocalState2[MainLobbyTab]

  case class Props(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback
  )

  val MainTabNav = new TabNav[MainLobbyTab]

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

        LocalBool.syncedWithSessionStorage(key = "is-admin", defaultValue = false) { isAdmin =>
          LocalString.syncedWithLocalStorage(key = "profile", defaultValue = "") { userName =>
            <.div(S.lobbyContainer, S.spaceyContainer)(
              profileSelector(isAdmin = isAdmin, userName = userName), {
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

                <.div(c"card")(
                  MainTabNav.make(
                    "main-tab",
                    MainLobbyTab.all,
                    MainLobbyTab.Debates,
                    Map(MainLobbyTab.Debates -> numDebatesMyTurn)
                  ) { mainTab =>
                    mainTab.value match {
                      case MainLobbyTab.Debates =>
                        DebatesPanel(
                          isAdmin = isAdmin.value,
                          lobby = lobby,
                          userName = userName.value,
                          connect = connect,
                          sendToMainChannel = sendToMainChannel
                        )
                      case MainLobbyTab.Leaderboard =>
                        LeaderboardPanel(lobby)
                      case MainLobbyTab.Admin =>
                        AdminPanel(
                          lobby = lobby,
                          qualityService = qualityService,
                          userName = userName.value,
                          connect = connect,
                          sendToMainChannel = sendToMainChannel
                        )
                    }
                  }
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
