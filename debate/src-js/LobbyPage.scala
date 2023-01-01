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
import monocle.macros.Lenses
import io.circe.Encoder
import io.circe.Decoder

class TabNav[A: Encoder: Decoder] {

  val LocalTab = new LocalState2[A]

  @Lenses
  case class Props(
    key: String,
    allTabs: Vector[A],
    initialTab: A,
    notifications: Map[A, Int],
    render: StateSnapshot[A] => VdomElement
  )

  import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  def make(key: String, allTabs: Vector[A], initialTab: A, notifications: Map[A, Int] = Map())(
    render: StateSnapshot[A] => VdomElement
  ) = Component(Props(key, allTabs, initialTab, notifications, render))

  val Component =
    ScalaComponent
      .builder[Props]("Tab Nav")
      .render_P { props =>
        LocalTab.syncedWithSessionStorage(props.key, props.initialTab) { tabState =>
          ReactFragment(
            <.div(c"card-header")(
              <.ul(c"nav nav-fill nav-tabs card-header-tabs")(
                props
                  .allTabs
                  .toVdomArray(tab =>
                    <.li(c"nav-item")(
                      ^.key := tab.toString,
                      <.a(^.classSet1("nav-link", "active" -> (tab == tabState.value)))(
                        ^.href := "#",
                        ^.onClick --> tabState.setState(tab),
                        tab.toString,
                        props
                          .notifications
                          .get(tab)
                          .filter(_ > 0)
                          .map { numNotifs =>
                            <.span(c"badge badge-danger badge-pill")(
                              ^.marginLeft  := "0.5rem",
                              ^.marginRight := "-0.5rem",
                              numNotifs
                            )
                          }
                      )
                    )
                  )
              )
            ),
            props.render(tabState)
          )
        }
      }
      .build

}

object LobbyPage {
  import Helpers.ClassSetInterpolator
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

  @JsonCodec
  sealed trait DebatesTab extends Product with Serializable {
    import DebatesTab._
    override def toString =
      this match {
        case MyDebates =>
          "My Official Debates"
        case AllOfficialDebates =>
          "All Official Debates"
        case PracticeDebates =>
          "Practice Debates"
      }
  }
  object DebatesTab {
    case object MyDebates          extends DebatesTab
    case object AllOfficialDebates extends DebatesTab
    case object PracticeDebates    extends DebatesTab

    def all = Vector(MyDebates, AllOfficialDebates, PracticeDebates)
  }

  @JsonCodec
  sealed trait AdminTab extends Product with Serializable {
    import AdminTab._
    override def toString =
      this match {
        case Profiles =>
          "Profiles"
        case CreateDebate =>
          "Create Debate"
      }
  }
  object AdminTab {
    case object Profiles     extends AdminTab
    case object CreateDebate extends AdminTab

    def all: Vector[AdminTab] = Vector(Profiles, CreateDebate)
  }

  val LocalBool   = new LocalState2[Boolean]
  val LocalString = new LocalState2[String]

  val LocalMainTab = new LocalState2[MainLobbyTab]

  val DebateTabNav = new TabNav[DebatesTab]
  val AdminTabNav  = new TabNav[AdminTab]

  def debatesSubtabs(
    isAdmin: Boolean,
    lobby: Lobby,
    userName: String,
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit]
  ) = {

    val myDebates = lobby.officialRooms.filter(_.roleAssignments.values.toSet.contains(userName))

    DebateTabNav.make("debate-tab", DebatesTab.all, DebatesTab.MyDebates) { tab =>
      import DebatesTab._
      val isOfficial = tab.value != PracticeDebates
      val currentRooms =
        tab.value match {
          case MyDebates =>
            myDebates
          case AllOfficialDebates =>
            lobby.officialRooms
          case PracticeDebates =>
            lobby.practiceRooms
        }

      <.div(c"card-body", S.spaceySubcontainer)(
        LocalString.syncedWithSessionStorage("room-name-search", "") { roomNameLive =>
          val canEnter =
            roomNameLive.value.nonEmpty && userName.nonEmpty &&
              currentRooms.exists(_.name == roomNameLive.value)
          val enter =
            if (canEnter)
              connect(ConnectionSpec(isOfficial, roomNameLive.value, userName))
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
                  def showRooms(rooms: Set[RoomMetadata], matches: Boolean) = rooms
                    .toVector
                    .sorted(RoomMetadata.getOrdering(userName))
                    .toVdomArray { case rm: RoomMetadata =>
                      MetadataBox(
                        isAdmin = isAdmin,
                        roomMetadata = rm,
                        isOfficial = isOfficial,
                        userName = userName,
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
      )
    }
  }

  def adminSubtabs(
    lobby: Lobby,
    qualityService: QuALITYService[AsyncCallback],
    userName: String,
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => Callback
  ) =
    AdminTabNav.make("admit-tab", AdminTab.all, AdminTab.CreateDebate) { tab =>
      import AdminTab._
      <.div(c"card-body", S.spaceySubcontainer)(
        tab.value match {
          case Profiles =>
            ReactFragment(
              <.h3("Active Profiles"),
              lobby
                .trackedDebaters
                .toVdomArray { name =>
                  <.div(<.i(c"bi bi-x", ^.onClick --> sendToMainChannel(RemoveDebater(name))), name)
                }
            )
          // LocalString.make("") { }
          // Helpers.textInputWithEnterButton()
          case CreateDebate =>
            FacilitatorPanel(
              lobby = lobby,
              qualityService = qualityService,
              joinDebate = Option(userName)
                .filter(_.nonEmpty)
                .map(userName =>
                  (isOfficial: Boolean, roomName: String) =>
                    connect(ConnectionSpec(isOfficial, roomName, userName))
                ),
              initDebate = sendToMainChannel
            )
        }
      )
    }

  case class Props(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback
  )

  val MainTabNav        = new TabNav[MainLobbyTab]
  val LeaderboardTabNav = new TabNav[LeaderboardCategory]

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
              deleteProfileButton(userName).when(isAdmin.value), {
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
                        debatesSubtabs(
                          isAdmin = isAdmin.value,
                          lobby = lobby,
                          userName = userName.value,
                          connect = connect,
                          sendToMainChannel = sendToMainChannel
                        )
                      case MainLobbyTab.Leaderboard =>
                        LeaderboardTabNav.make(
                          "leaderboard-tab",
                          LeaderboardCategory.all,
                          LeaderboardCategory.Judge
                        ) { leaderboardTab =>
                          <.div(c"card-body", S.spaceySubcontainer)(
                            LeaderboardTable.makeSingle(
                              lobby.trackedDebaters,
                              lobby.leaderboard,
                              leaderboardTab.value
                            )
                          )
                        }
                      case MainLobbyTab.Admin =>
                        adminSubtabs(
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
      .componentDidMount(_ => Callback(dom.window.document.title = Helpers.makePageTitle("Lobby")))
      .build

  def make(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback
  ) = Component(Props(qualityService, lobby, sendToMainChannel, connect))
}
