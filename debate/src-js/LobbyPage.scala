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

  import LobbyTab._

  def lobbySelector(lobbyTab: StateSnapshot[LobbyTab]) = List(
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
    )(^.href := "#", ^.onClick --> lobbyTab.setState(tab), tab.toString)
  )

  @JsonCodec
  sealed trait LobbyTab extends Product with Serializable {
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

  def roomNameInput(roomNameLive: StateSnapshot[String], enter: Callback) =
    V.LiveTextField
      .String
      .modInput(input =
        TagMod(
          c"form-control",
          ^.onKeyDown ==>
            ((e: ReactKeyboardEvent) =>
              if (e.keyCode == dom.ext.KeyCode.Enter)
                enter
              else
                Callback.empty
            )
        )
      )(roomNameLive, placeholderOpt = Some("Room"))

  def makeMetadatas(
    visibleRooms: Vector[RoomMetadata],
    r: RoomStatus,
    enter: Callback,
    isOfficial: Boolean,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    userName: StateSnapshot[String]
  ) = {
    val statusStyle = {
      import RoomStatus._
      r match {
        case SettingUp =>
          S.settingUpStatusLabel
        case InProgress =>
          S.inProgressStatusLabel
        case Complete =>
          S.completeStatusLabel
        case WaitingToBegin =>
          S.waitingToBeginStatusLabel
      }
    }

    <.div(S.debateMetadataContainer)(
      <.h5(statusStyle)(r.toStringForTitle),
      visibleRooms
        .filter(_.status == r)
        .toVdomArray { rm =>
          // TODO add keys? rm.name
          DebateMetadata.make(
            roomMetadata = rm,
            isOfficial = isOfficial,
            userName = userName,
            sendToMainChannel = sendToMainChannel,
            enterRoom = enter
          )

        }
    )
  }

  val noProfileString = "(select profile)"
  // TODO rename these
  def setChoice(userName: StateSnapshot[String])(name: String) = {
    val adjustedName =
      if (name == noProfileString)
        ""
      else
        name
    userName.setState(adjustedName)
  }

  def currentChoice(userName: StateSnapshot[String], lobby: StateSnapshot[Lobby]) =
    if (lobby.value.trackedDebaters.contains(userName.value)) {
      userName.value
    } else
      noProfileString

  def createProfileButton(
    userName: StateSnapshot[String],
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    lobby: StateSnapshot[Lobby]
  ) =
    <.div(c"form-group row", Styles.adminOnly) {
      val name       = userName.value
      val isDisabled = (lobby.value.trackedDebaters + "" + "(no profile)").contains(name)
      <.button(c"btn btn-primary btn-block")(
        "Create profile",
        ^.disabled := isDisabled,
        (^.onClick --> sendToMainChannel(RegisterDebater(userName.value))).when(!isDisabled)
      )
    }

  def deleteProfileButton(
    userName: StateSnapshot[String],
    lobby: StateSnapshot[Lobby],
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit]
  ) =
    <.div(c"form-group row", Styles.adminOnly) {
      val name      = userName.value
      val isEnabled = lobby.value.trackedDebaters.contains(name)
      <.button(c"btn btn-danger btn-block")(
        "Delete profile",
        ^.disabled := !isEnabled,
        (^.onClick --> sendToMainChannel(RemoveDebater(userName.value))).when(isEnabled)
      )
    }

  val LocalString = new LocalState2[String]

  val LocalLobbyTab = new LocalState2[LobbyTab]

  def profileSelector(userName: StateSnapshot[String], lobby: StateSnapshot[Lobby]) =
    <.div(c"form-group row")(
      <.label(c"col-sm-2 col-form-label")("Profile:"),
      V.Select
        .String
        .modFull(TagMod(c"col-sm-10", S.customSelect))(
          choices = noProfileString +: lobby.value.trackedDebaters.toList.sorted,
          curChoice = currentChoice(userName, lobby = lobby),
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

  def roomNameSearch(
    userName: StateSnapshot[String],
    isOfficial: Boolean,
    currentRooms: Vector[RoomMetadata],
    props: Props
  ) =
    LocalString.syncedWithSessionStorage("room-name-search", "") { roomNameLive =>
      val canEnter =
        roomNameLive.value.nonEmpty && userName.value.nonEmpty &&
          currentRooms.exists(_.name == roomNameLive.value)
      val enter =
        if (canEnter)
          props.connect(ConnectionSpec(isOfficial, roomNameLive.value, userName.value))
        else
          Callback.empty

      val visibleRooms = currentRooms.filter(_.matchesQuery(roomNameLive.value))

      def ourMakeMetadatas(r: RoomStatus) = makeMetadatas(
        visibleRooms = visibleRooms,
        r = r,
        enter = enter,
        isOfficial = isOfficial,
        sendToMainChannel = props.sendToMainChannel,
        userName = userName
      )

      ReactFragment(
        Helpers.textInputWithEnterButton(
          field = roomNameLive,
          placeholderOpt = Some("Room"),
          buttonText = "Join",
          isEnabled = canEnter,
          enter = enter
        ),
        Option(<.div("No rooms to show.")).filter(_ => visibleRooms.isEmpty),
        ourMakeMetadatas(r = RoomStatus.SettingUp),
        ourMakeMetadatas(RoomStatus.WaitingToBegin),
        ourMakeMetadatas(RoomStatus.InProgress),
        ourMakeMetadatas(RoomStatus.Complete)
      )
    }

  def localLobbyTab(lobby: StateSnapshot[Lobby], userName: StateSnapshot[String], props: Props) =
    LocalLobbyTab.syncedWithSessionStorage(key = "lobby-tab", defaultValue = LobbyTab.MyDebates) {
      lobbyTab =>
        val myDebates = lobby
          .value
          .officialRooms
          .filter(_.assignedParticipants.values.toSet.contains(userName.value))
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

        <.div(c"card", ^.textAlign.center)(
          <.div(c"card-header")(
            <.ul(c"nav nav-fill nav-tabs card-header-tabs")(lobbySelector(lobbyTab))
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
                        props.connect(ConnectionSpec(isOfficial, roomName, userName))
                    ),
                  profiles = lobby.value.trackedDebaters,
                  qualityService = props.qualityService,
                  initDebate = props.sendToMainChannel
                )
              case _ =>
                roomNameSearch(
                  userName = userName,
                  isOfficial = isOfficial,
                  currentRooms = currentRooms,
                  props = props
                )
            }
          )
        )

      // TODO what about debates with the same name?
    }

  val Component =
    ScalaComponent
      .builder[Props]("Lobby Page")
      .render_P { case props @ Props(qualityService, lobby, sendToMainChannel, connect) =>
        LocalString.syncedWithLocalStorage(key = "profile", defaultValue = "") { userName =>
          <.div(S.lobbyContainer, S.spaceyContainer)(
            profileSelector(userName = userName, lobby = lobby),
            nameInput(userName = userName),
            createProfileButton(userName, lobby = lobby, sendToMainChannel = sendToMainChannel),
            deleteProfileButton(userName, lobby = lobby, sendToMainChannel = sendToMainChannel),
            localLobbyTab(lobby = lobby, userName = userName, props = props)
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
