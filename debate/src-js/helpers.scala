package debate
// TODO rename this file

import org.scalajs.dom

import scala.language.existentials // see https://github.com/suzaku-io/diode/issues/50

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot

import scalacss.ScalaCssReact._

import cats.Foldable
import cats.Functor
import cats.implicits._

import debate.RoomMetadata

case class ConnectionSpec(
    isOfficial: Boolean,
    roomName: String,
    participantName: String
)

object Helpers {
  val S = Styles
  val V = new jjm.ui.View(S)

  // TODO duplicated
  implicit class ClassSetInterpolator(val sc: StringContext) extends AnyVal {
    def c(args: Any*) = {
      // concatenate everything: use the built-in S method (which happens to be used in the S interpolator)
      ^.classSet1(sc.s(args: _*))
    }
  }

  def commaSeparatedSpans[F[_]: Foldable: Functor](fa: F[String]) = {
    fa.map(x => Vector(<.span(x))).intercalate(Vector(<.span(", ")))
  }

  def makeLobbyPageWhenNotConnected(
      lobby: StateSnapshot[Lobby],
      connectionSpecOpt: StateSnapshot[Option[ConnectionSpec]],
      sendToMainChannel: debate.MainChannelRequest => japgolly.scalajs.react.CallbackTo[
        Unit
      ]
  ) = {
    def enterRoom(
        isOfficial: Boolean,
        roomName: String,
        participantName: String
    ) =
      connectionSpecOpt.setState(
        Some(
          ConnectionSpec(isOfficial, roomName, participantName)
        )
      )
    // TODO change page title? maybe do this on mount for the debate room component instead
    // >> Callback(dom.window.document.title = makePageTitle(roomName)) >>
    // Callback(dom.window.history.replaceState("", makePageTitle(roomName), roomName))
    // roomName.setState(roomNameLive.value)

    val noProfileString = "(select profile)"
    val profileCookieId = "debate-profile"

    def setChoice(
        userName: StateSnapshot[String]
    )(name: String) = {
      val adjustedName =
        if (name == noProfileString) "" else name
      userName.setState(adjustedName) >> Callback(
        setCookie(
          profileCookieId,
          adjustedName,
          expires = 5
        )
      )
    }

    def curChoice(userName: StateSnapshot[String]) = {
      if (
        lobby.value.trackedDebaters
          .contains(userName.value)
      ) {
        userName.value
      } else noProfileString
    }

    def createProfileButton(userName: StateSnapshot[String]) = {
      <.div(c"form-group row", Styles.adminOnly) {
        val name = userName.value
        val isDisabled =
          (lobby.value.trackedDebaters + "" + "(no profile)")
            .contains(name)
        <.button(c"btn btn-primary btn-block")(
          "Create profile",
          ^.disabled := isDisabled,
          (^.onClick --> sendToMainChannel(
            RegisterDebater(userName.value)
          )).when(!isDisabled)
        )
      }
    }

    def deleteProfileButton(
        userName: StateSnapshot[String]
    ) = {
      <.div(c"form-group row", Styles.adminOnly) {
        val name = userName.value
        val isEnabled =
          lobby.value.trackedDebaters.contains(name)
        <.button(c"btn btn-danger btn-block")(
          "Delete profile",
          ^.disabled := !isEnabled,
          (^.onClick --> sendToMainChannel(
            RemoveDebater(userName.value)
          )).when(isEnabled)
        )
      }
    }

    def joinOrCreateDebate(
        currentRooms: Vector[RoomMetadata],
        isOfficial: Boolean,
        canEnter: Boolean,
        enter: Callback,
        roomNameLive: StateSnapshot[String]
    ) = {
      <.div(c"input-group-append")(
        <.button(c"btn btn-primary")(
          (
            (if (
               currentRooms.exists(
                 _.name == roomNameLive.value
               )
             ) "Join"
             else "Create") +
              " " +
              (if (isOfficial) "Official Debate"
               else "Practice Debate")
          ),
          ^.`type` := "button",
          ^.disabled := !canEnter,
          ^.onClick --> enter
        )
      )
    }

    def roomManagement(
        roomMetadata: RoomMetadata,
        isOfficial: Boolean,
        userName: StateSnapshot[String]
    ) = {
      val canEnterRoom =
        userName.value.nonEmpty && !roomMetadata.currentParticipants
          .contains(userName.value)
      val statusStyle = {
        import RoomStatus._
        roomMetadata.status match {
          case SettingUp => S.settingUpStatusLabel
          case InProgress =>
            S.inProgressStatusLabel
          case Complete => S.completeStatusLabel
        }
      }
      val selectableStyle =
        if (canEnterRoom) S.simpleSelectable
        else S.simpleUnselectable
      <.div(S.optionBox, selectableStyle)(
        <.div(S.optionTitle)(
          roomMetadata.name,
          " ",
          <.span(statusStyle)(s"($roomMetadata.status)")
        ),
        <.div(
          <.strong("Assigned: "),
          commaSeparatedSpans(
            roomMetadata.assignedParticipants.toList.sorted
          ).toVdomArray
        ).when(roomMetadata.assignedParticipants.nonEmpty),
        <.div(
          <.strong("Present: "),
          commaSeparatedSpans(
            roomMetadata.currentParticipants.toList.sorted
          ).toVdomArray
        ).when(roomMetadata.currentParticipants.nonEmpty),
        <.button(
          c"btn btn-block btn-danger",
          S.adminOnly
        )(
          "Delete room",
          ^.onClick ==> ((e: ReactMouseEvent) => {
            e.stopPropagation();
            sendToMainChannel(
              DeleteRoom(isOfficial, roomMetadata.name)
            )
          })
        ),
        (^.onClick --> enterRoom(
          isOfficial,
          roomMetadata.name,
          userName.value
        )).when(canEnterRoom)
      )
    }

    import jjm.ui.LocalState

    sealed trait LobbyTab extends Product with Serializable {
      import LobbyTab._
      override def toString = this match {
        case MyDebates          => "My Debates"
        case AllOfficialDebates => "All Official Debates"
        case PracticeDebates    => "Practice Debates"
      }
    }
    object LobbyTab {
      case object MyDebates extends LobbyTab
      case object AllOfficialDebates extends LobbyTab
      case object PracticeDebates extends LobbyTab
    }

    val LocalString = new LocalState[String]

    val LocalLobbyTab = new LocalState[LobbyTab]
    LocalString.make(initialValue = getCookie(profileCookieId).getOrElse("")) {
      userName =>
        <.div(S.lobbyContainer, S.spaceyContainer)(
          <.div(c"form-group row")(
            <.label(c"col-sm-2 col-form-label")("Profile:"),
            V.Select.String.modFull(
              TagMod(c"col-sm-10", S.customSelect)
            )(
              choices =
                noProfileString +: lobby.value.trackedDebaters.toList.sorted,
              curChoice = curChoice(userName),
              setChoice = setChoice(userName)
            )
          ),
          V.LiveTextField.String.mod(
            span = TagMod(c"form-group row", Styles.adminOnly),
            label = c"col-sm-2 col-form-label",
            input = c"col-sm-10 form-control"
          )(userName, labelOpt = Some("Name: ")),
          createProfileButton(userName),
          deleteProfileButton(userName),
          LocalLobbyTab.make(LobbyTab.MyDebates) { lobbyTab =>
            import LobbyTab._
            val myDebates = lobby.value.officialRooms
              .filter(
                _.assignedParticipants.contains(userName.value)
              )
            val isOfficial = lobbyTab.value match {
              case PracticeDebates => false
              case _               => true
            }
            val currentRooms = lobbyTab.value match {
              case MyDebates          => myDebates
              case AllOfficialDebates => lobby.value.officialRooms
              case PracticeDebates    => lobby.value.practiceRooms
            }
            <.div(c"card", ^.textAlign.center)(
              <.div(c"card-header")(
                <.ul(c"nav nav-fill nav-tabs card-header-tabs")(
                  List(
                    MyDebates,
                    AllOfficialDebates,
                    PracticeDebates
                  ).toVdomArray(tab =>
                    <.li(c"nav-item")(
                      <.a(
                        ^.classSet1(
                          "nav-link",
                          "active" -> (tab == lobbyTab.value)
                        )
                      )(
                        ^.href := "#",
                        ^.onClick --> lobbyTab.setState(tab),
                        tab.toString
                      )
                    )
                  )
                )
              ),
              LocalString.make("") { roomNameLive =>
                val canEnter =
                  roomNameLive.value.nonEmpty && userName.value.nonEmpty
                val enter =
                  if (canEnter)
                    enterRoom(
                      isOfficial,
                      roomNameLive.value,
                      userName.value
                    )
                  else Callback.empty
                <.div(c"card-body", S.spaceySubcontainer)(
                  <.div(c"input-group", ^.width.auto)(
                    V.LiveTextField.String.modInput(
                      input = TagMod(
                        c"form-control",
                        ^.onKeyDown ==> ((e: ReactKeyboardEvent) =>
                          if (e.keyCode == dom.ext.KeyCode.Enter) enter
                          else Callback.empty
                        )
                      )
                    )(
                      roomNameLive,
                      placeholderOpt = Some("Room")
                    ),
                    joinOrCreateDebate(
                      currentRooms = currentRooms,
                      isOfficial = isOfficial,
                      canEnter = canEnter,
                      enter = enter,
                      roomNameLive = roomNameLive
                    )
                  ).when(lobbyTab.value != MyDebates),
                  <.div("No rooms to show.")
                    .when(currentRooms.isEmpty),
                  currentRooms.toVdomArray { case rm: RoomMetadata =>
                    roomManagement(
                      roomMetadata = rm,
                      isOfficial = isOfficial,
                      userName = userName
                    ) // TODO right name?
                  }
                )
              }
            )
          }
        )
    }

  }
}
