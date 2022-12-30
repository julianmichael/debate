package debate
import cats.implicits._

import io.circe.generic.JsonCodec
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._

import debate.util.LocalState2
import japgolly.scalajs.react.feature.ReactFragment
import debate.quality.QuALITYService

object LobbyPage {
  import Helpers.ClassSetInterpolator
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

        def createProfileButton(userName: StateSnapshot[String]) =
          <.div(c"form-group row") {
            val name       = userName.value
            val isDisabled = (lobby.trackedDebaters + "" + "(no profile)").contains(name)
            <.button(c"btn btn-primary btn-block")(
              "Create profile",
              ^.disabled := isDisabled,
              (^.onClick --> sendToMainChannel(RegisterDebater(userName.value))).when(!isDisabled)
            )
          }

        def deleteProfileButton(userName: StateSnapshot[String]) =
          <.div(c"form-group row") {
            val name      = userName.value
            val isEnabled = lobby.trackedDebaters.contains(name)
            <.button(c"btn btn-danger btn-block")(
              "Delete profile",
              ^.disabled := !isEnabled,
              (^.onClick --> sendToMainChannel(RemoveDebater(userName.value))).when(isEnabled)
            )
          }

        @JsonCodec
        sealed trait LobbyTab extends Product with Serializable {
          import LobbyTab._
          override def toString =
            this match {
              case MyDebates =>
                "My Debates"
              case AllOfficialDebates =>
                "All Official Debates"
              case PracticeDebates =>
                "Practice Debates"
              case Leaderboard =>
                "Leaderboard"
              case CreateDebates =>
                "Create Debates"
            }
        }
        object LobbyTab {
          case object MyDebates          extends LobbyTab
          case object AllOfficialDebates extends LobbyTab
          case object PracticeDebates    extends LobbyTab
          case object Leaderboard        extends LobbyTab
          case object CreateDebates      extends LobbyTab
        }

        val LocalBool   = new LocalState2[Boolean]
        val LocalString = new LocalState2[String]

        val LocalLobbyTab = new LocalState2[LobbyTab]

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

        def nameInput(userName: StateSnapshot[String]) =
          V.LiveTextField
            .String
            .mod(
              span = TagMod(c"form-group row"),
              label = c"col-sm-2 col-form-label",
              input = c"col-sm-10 form-control"
            )(userName, labelOpt = Some("Name: "))

        LocalBool.syncedWithSessionStorage(key = "is-admin", defaultValue = false) { isAdmin =>
          LocalString.syncedWithLocalStorage(key = "profile", defaultValue = "") { userName =>
            <.div(S.lobbyContainer, S.spaceyContainer)(
              profileSelector(isAdmin = isAdmin, userName = userName),
              nameInput(userName = userName).when(isAdmin.value),
              createProfileButton(userName).when(isAdmin.value),
              deleteProfileButton(userName).when(isAdmin.value),
              LocalLobbyTab.syncedWithSessionStorage(
                key = "lobby-tab",
                defaultValue = LobbyTab.MyDebates
              ) { lobbyTab =>
                import LobbyTab._
                val myDebates = lobby
                  .officialRooms
                  .filter(_.roleAssignments.values.toSet.contains(userName.value))
                val isOfficial =
                  lobbyTab.value match {
                    case PracticeDebates =>
                      false
                    case _ =>
                      true
                  }
                val currentRooms =
                  lobbyTab.value match {
                    case MyDebates =>
                      myDebates
                    case AllOfficialDebates =>
                      lobby.officialRooms
                    case PracticeDebates =>
                      lobby.practiceRooms
                    case Leaderboard =>
                      Set[RoomMetadata]()
                    case CreateDebates =>
                      Set[RoomMetadata]()
                  }

                val numDebatesMyTurn =
                  lobby
                    .officialRooms
                    .filter { room =>
                      val myRoles = room.roleAssignments.filter(_._2 == userName.value).keySet
                      myRoles.intersect(room.currentSpeakers).nonEmpty
                    }
                    .size

                def lobbySelector() = List(
                  MyDebates,
                  AllOfficialDebates,
                  PracticeDebates,
                  Leaderboard,
                  CreateDebates
                ).toVdomArray(tab =>
                  <.li(c"nav-item")(
                    ^.key := tab.toString,
                    <.a(^.classSet1("nav-link", "active" -> (tab == lobbyTab.value)))(
                      ^.href := "#",
                      ^.onClick --> lobbyTab.setState(tab),
                      tab.toString,
                      <.span(c"badge badge-danger badge-pill")(
                          ^.marginLeft  := "0.5rem",
                          ^.marginRight := "-0.5rem",
                          numDebatesMyTurn
                        )
                        .when(tab == MyDebates && numDebatesMyTurn > 0)
                    )
                  )
                )

                <.div(c"card")(
                  <.div(c"card-header")(
                    <.ul(c"nav nav-fill nav-tabs card-header-tabs")(lobbySelector())
                  ),
                  <.div(c"card-body", S.spaceySubcontainer)(
                    lobbyTab.value match {
                      case LobbyTab.Leaderboard =>
                        LeaderboardTable
                          .make(lobby.leaderboard)
                          .when(lobbyTab.value == LobbyTab.Leaderboard)
                      case LobbyTab.CreateDebates =>
                        FacilitatorPanel(
                          lobby = lobby,
                          joinDebate = Option(userName.value)
                            .filter(_.nonEmpty)
                            .map(userName =>
                              (isOfficial: Boolean, roomName: String) =>
                                connect(ConnectionSpec(isOfficial, roomName, userName))
                            ),
                          profiles = lobby.trackedDebaters,
                          qualityService = qualityService,
                          initDebate = sendToMainChannel
                        )
                      case _ =>
                        LocalString.syncedWithSessionStorage("room-name-search", "") {
                          roomNameLive =>
                            val canEnter =
                              roomNameLive.value.nonEmpty && userName.value.nonEmpty &&
                                currentRooms.exists(_.name == roomNameLive.value)
                            val enter =
                              if (canEnter)
                                connect(
                                  ConnectionSpec(isOfficial, roomNameLive.value, userName.value)
                                )
                              else
                                Callback.empty

                            val (matchingRooms, nonMatchingRooms) =
                              if (roomNameLive.value.isEmpty)
                                currentRooms -> Set[RoomMetadata]()
                              else
                                currentRooms.partition(_.matchesQuery(roomNameLive.value))

                            def makeMetadatas(status: RoomStatus) = {
                              val statusStyle = {
                                import RoomStatus._
                                status match {
                                  case WaitingToBegin =>
                                    S.waitingToBeginStatusLabel
                                  case InProgress =>
                                    S.inProgressStatusLabel
                                  case Complete =>
                                    S.completeStatusLabel
                                }
                              }
                              val hasRooms = currentRooms.exists(_.status == status)
                              ReactFragment(
                                <.h5(statusStyle)(status.titleString),
                                <.div(S.metadataListContainer, S.spaceySubcontainer)(
                                  if (!hasRooms) {
                                    <.div("No rooms to show.")
                                  } else {
                                    def showRooms(rooms: Set[RoomMetadata], matches: Boolean) =
                                      rooms
                                        .toVector
                                        .sorted(RoomMetadata.getOrdering(userName.value))
                                        .toVdomArray { case rm: RoomMetadata =>
                                          MetadataBox(
                                            isAdmin = isAdmin.value,
                                            roomMetadata = rm,
                                            isOfficial = isOfficial,
                                            userName = userName.value,
                                            sendToMainChannel = sendToMainChannel,
                                            enterRoom = connect
                                          )(^.key := rm.name, (^.opacity := "0.25").when(!matches))
                                        }

                                    ReactFragment(
                                      showRooms(matchingRooms.filter(_.status == status), true),
                                      showRooms(nonMatchingRooms.filter(_.status == status), false)
                                    )
                                  }
                                )
                              )
                            }

                            ReactFragment(
                              Helpers.textInputWithEnterButton(
                                field = roomNameLive,
                                placeholderOpt = Some("Room"),
                                buttonText = "Join",
                                isEnabled = canEnter,
                                enter = enter
                              )(^.marginBottom := 1.rem),
                              makeMetadatas(RoomStatus.InProgress),
                              <.div(<.hr),
                              makeMetadatas(RoomStatus.WaitingToBegin),
                              <.div(<.hr),
                              makeMetadatas(RoomStatus.Complete)
                            )
                        }
                    }
                  )
                )
              }
            )
          }
        }
      }
      .componentDidMount(_ => Callback(dom.window.document.title = Helpers.makePageTitle("Lobby")))
      .build

  def make(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback
  ) = Component(Props(qualityService, lobby, sendToMainChannel, connect))
}
