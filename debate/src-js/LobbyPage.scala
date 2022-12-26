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
    lobby: StateSnapshot[Lobby],
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
          if (lobby.value.trackedDebaters.contains(userName.value)) {
            userName.value
          } else
            noProfileString

        def createProfileButton(userName: StateSnapshot[String]) =
          <.div(c"form-group row", Styles.adminOnly) {
            val name       = userName.value
            val isDisabled = (lobby.value.trackedDebaters + "" + "(no profile)").contains(name)
            <.button(c"btn btn-primary btn-block")(
              "Create profile",
              ^.disabled := isDisabled,
              (^.onClick --> sendToMainChannel(RegisterDebater(userName.value))).when(!isDisabled)
            )
          }

        def deleteProfileButton(userName: StateSnapshot[String]) =
          <.div(c"form-group row", Styles.adminOnly) {
            val name      = userName.value
            val isEnabled = lobby.value.trackedDebaters.contains(name)
            <.button(c"btn btn-danger btn-block")(
              "Delete profile",
              ^.disabled := !isEnabled,
              (^.onClick --> sendToMainChannel(RemoveDebater(userName.value))).when(isEnabled)
            )
          }

        def roomManagement(
          roomMetadata: RoomMetadata,
          isOfficial: Boolean,
          userName: StateSnapshot[String]
        ) = {
          val canEnterRoom =
            userName.value.nonEmpty && !roomMetadata.currentParticipants.contains(userName.value)
          val statusStyle = {
            import RoomStatus._
            roomMetadata.status match {
              case InProgress =>
                S.inProgressStatusLabel
              case Complete =>
                S.completeStatusLabel
            }
          }

          // TODO probably just a val instead of a def
          def statusDisplay(status: debate.RoomStatus) =
            <.div(S.optionTitle)(roomMetadata.name, " ", <.span(statusStyle)(s"($status)"))

          def assignedParticipants() = <
            .div(
              <.strong("Assigned: "),
              Helpers
                .commaSeparatedSpans(roomMetadata.assignedParticipants.toList.sorted)
                .toVdomArray
            )
            .when(roomMetadata.assignedParticipants.nonEmpty)

          def presentParticipants() = <
            .div(
              <.strong("Present: "),
              Helpers
                .commaSeparatedSpans(roomMetadata.currentParticipants.toList.sorted)
                .toVdomArray
            )
            .when(roomMetadata.currentParticipants.nonEmpty)

          def deleteRoom() =
            <.button(c"btn btn-block btn-danger", S.adminOnly)(
              "Delete room",
              ^.onClick ==>
                ((e: ReactMouseEvent) => {
                  e.stopPropagation();
                  sendToMainChannel(DeleteRoom(isOfficial, roomMetadata.name))
                })
            )

          def enterRoomButton() =
            (^.onClick --> connect(ConnectionSpec(isOfficial, roomMetadata.name, userName.value)))
              .when(canEnterRoom)

          val selectableStyle =
            if (canEnterRoom)
              S.simpleSelectable
            else
              S.simpleUnselectable
          val status = roomMetadata.status
          <.div(S.optionBox, selectableStyle)(
            statusDisplay(status = status),
            assignedParticipants(),
            presentParticipants(),
            deleteRoom(),
            enterRoomButton()
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

        val LocalString = new LocalState2[String]

        val LocalLobbyTab = new LocalState2[LobbyTab]

        def profileSelector(userName: StateSnapshot[String]) =
          <.div(c"form-group row")(
            <.label(c"col-sm-2 col-form-label")("Profile:"),
            V.Select
              .String
              .modFull(TagMod(c"col-sm-10", S.customSelect))(
                choices = noProfileString +: lobby.value.trackedDebaters.toList.sorted,
                curChoice = curChoice(userName),
                setChoice = setChoice(userName)
              )
          )

        def nameInput(userName: StateSnapshot[String]) =
          V.LiveTextField
            .String
            .mod(
              span = TagMod(c"form-group row", Styles.adminOnly),
              label = c"col-sm-2 col-form-label",
              input = c"col-sm-10 form-control"
            )(userName, labelOpt = Some("Name: "))

        LocalString.syncedWithLocalStorage(key = "profile", defaultValue = "") { userName =>
          <.div(S.lobbyContainer, S.spaceyContainer)(
            profileSelector(userName = userName),
            nameInput(userName = userName),
            createProfileButton(userName),
            deleteProfileButton(userName),
            LocalLobbyTab.syncedWithSessionStorage(
              key = "lobby-tab",
              defaultValue = LobbyTab.MyDebates
            ) { lobbyTab =>
              import LobbyTab._
              val myDebates = lobby
                .value
                .officialRooms
                .filter(_.assignedParticipants.contains(userName.value))
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
                    lobby.value.officialRooms
                  case PracticeDebates =>
                    lobby.value.practiceRooms
                  case Leaderboard =>
                    Vector.empty
                  case CreateDebates =>
                    Vector.empty
                }

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
                    tab.toString
                  )
                )
              )

              <.div(c"card", ^.textAlign.center)(
                <.div(c"card-header")(
                  <.ul(c"nav nav-fill nav-tabs card-header-tabs")(lobbySelector())
                ),
                <.div(c"card-body", S.spaceySubcontainer)(
                  lobbyTab.value match {
                    case LobbyTab.Leaderboard =>
                      LeaderboardTable
                        .make(lobby.value.leaderboard)
                        .when(lobbyTab.value == LobbyTab.Leaderboard)
                    case LobbyTab.CreateDebates =>
                      FacilitatorPanel(
                        lobby = lobby.value,
                        joinDebate = Option(userName.value)
                          .filter(_.nonEmpty)
                          .map(userName =>
                            (isOfficial: Boolean, roomName: String) =>
                              connect(ConnectionSpec(isOfficial, roomName, userName))
                          ),
                        profiles = lobby.value.trackedDebaters,
                        qualityService = qualityService,
                        initDebate = sendToMainChannel
                      )
                    case _ =>
                      LocalString.syncedWithSessionStorage("room-name-search", "") { roomNameLive =>
                        val canEnter =
                          roomNameLive.value.nonEmpty && userName.value.nonEmpty &&
                            currentRooms.exists(_.name == roomNameLive.value)
                        val enter =
                          if (canEnter)
                            connect(ConnectionSpec(isOfficial, roomNameLive.value, userName.value))
                          else
                            Callback.empty

                        val visibleRooms = currentRooms.filter(_.matchesQuery(roomNameLive.value))

                        ReactFragment(
                          Helpers.textInputWithEnterButton(
                            field = roomNameLive,
                            placeholderOpt = Some("Room"),
                            buttonText = "Join",
                            isEnabled = canEnter,
                            enter = enter
                          ),
                          Option(<.div("No rooms to show.")).filter(_ => currentRooms.isEmpty),
                          visibleRooms.toVdomArray { case rm: RoomMetadata =>
                            roomManagement(
                              roomMetadata = rm,
                              isOfficial = isOfficial,
                              userName = userName
                            )(^.key := rm.name)
                          }
                        )
                      }
                  }
                )
              )
            }
          )
        }
      }
      .componentDidMount(_ => Callback(dom.window.document.title = Helpers.makePageTitle("Lobby")))
      .build

  def make(
    qualityService: QuALITYService[AsyncCallback],
    lobby: StateSnapshot[Lobby],
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback
  ) = Component(Props(qualityService, lobby, sendToMainChannel, connect))
}
