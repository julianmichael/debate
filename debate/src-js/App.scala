package debate

import annotation.unused

import scalajs.js.typedarray.TypedArrayBuffer

import org.scalajs.dom

import org.scalajs.jquery.jQuery

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.MonocleReact._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import boopickle.Default._

import scala.util.Try

import cats.~>
import cats.Foldable
import cats.Functor
import cats.implicits._

import debate.util._
import scala.concurrent.Future
import jjm.io.HttpUtil
import jjm.OrWrapped

/** The main webapp. */
object App {

  implicit class ClassSetInterpolator(val sc: StringContext) extends AnyVal {
    def c(args: Any*) = {
      // concatenate everything: use the built-in S method (which happens to be used in the S interpolator)
      ^.classSet1(sc.s(args: _*))
    }
  }

  def commaSeparatedSpans[F[_]: Foldable: Functor](fa: F[String]) = {
    fa.map(x => Vector(<.span(x))).intercalate(Vector(<.span(", ")))
  }

  val wsProtocol = {
    if (dom.document.location.protocol == "https:") "wss:" else "ws:"
  }

  def boopickleWebsocket[A: Pickler, B: Pickler] = {
    // import scalajs.js.typedarray._
    import DataConversions._
    new WebSocketConnection2[A, B](
      sendRequest = (socket, req) =>
        Callback {
          socket.send(Pickle.intoBytes(req))
        },
      readResponse = x => {
        val res = Unpickle[B].fromBytes(TypedArrayBuffer.wrap(x));
        res
      }
    )
  }

  def boopickleSyncedState[Request: Pickler, State: Pickler](
      getRequestFromState: State => Request
  ) = {
    // import scalajs.js.typedarray._
    import DataConversions._
    new SyncedState[Request, State](
      sendRequest = (socket, req) =>
        Callback {
          socket.send(Pickle.intoBytes(req))
        },
      readResponse = x => {
        val res = Unpickle[State].fromBytes(TypedArrayBuffer.wrap(x));
        res
      },
      getRequestFromState = getRequestFromState
    )
  }

  val DebateWebSocket = boopickleWebsocket[DebateState, DebateState]
  val SyncedDebate =
    boopickleSyncedState[DebateStateUpdateRequest, DebateState](
      getRequestFromState = DebateStateUpdateRequest.State(_)
    )
  def getDebateWebsocketUri(
      isOfficial: Boolean,
      roomName: String,
      participantId: String
  ): String = {
    val prefix = if (isOfficial) "official" else "practice"
    s"$wsProtocol//${dom.document.location.host}/$prefix-ws/$roomName?name=$participantId"
  }

  val MainWebSocket = boopickleWebsocket[MainChannelRequest, Lobby]
  val mainWebsocketUri: String = {
    s"$wsProtocol//${dom.document.location.host}/main-ws"
  }

  val httpProtocol = dom.document.location.protocol
  val qualityApiUrl: String = {
    s"$httpProtocol//${dom.document.location.host}/$qualityServiceApiEndpoint"
  }
  println(mainWebsocketUri)
  println(qualityApiUrl)

  import scala.concurrent.ExecutionContext.Implicits.global

  type DelayedFuture[A] = () => Future[A]
  val toAsyncCallback = {
    λ[DelayedFuture ~> AsyncCallback](f => AsyncCallback.fromFuture(f()))
  }
  def wrap[F[_]] = λ[F ~> OrWrapped[F, *]] { f =>
    OrWrapped.wrapped(f)
  }

  val qualityStoryService = quality.QuALITYService(
    HttpUtil
      .makeHttpPostClient[quality.QuALITYService.Request](
        qualityApiUrl
      )
      .andThenK(toAsyncCallback)
  )

  import jjm.ui.LocalState

  case class ConnectionSpec(
      isOfficial: Boolean,
      roomName: String,
      participantName: String
  )

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

  val defaultRoomName: String =
    jQuery("#defaultRoomName").attr("value").toOption.getOrElse("")

  // Shortcuts for styles and view elements

  val S = Styles
  val V = new jjm.ui.View(S)

  // instantiate the HOCs we need

  val LocalLobbyTab = new LocalState[LobbyTab]
  val LocalString = new LocalState[String]
  val LocalDouble = new LocalState[Double]
  val LocalStringOpt = new LocalState[Option[String]]
  val LocalConnectionSpecOpt = new LocalState[Option[ConnectionSpec]]
  val LocalLobby = new LocalState[Lobby]

  val StringOptField = V.LiveTextField[Option[String]](
    x => Some(Option(x).filter(_.nonEmpty)),
    _.getOrElse("")
  )
  val IntOptField = V.LiveTextField[Option[Int]](
    x => if (x.isEmpty) Option(None) else Try(x.toInt).toOption.map(Option(_)),
    _.foldMap(_.toString)
  )

  val debatePanel = new DebatePanel(S, V)
  val facilitatorPanel = new FacilitatorPanel(S, V)

  /** Top row showing non-debating roles (user can click one to change roles).
    */
  def headerRow(
      isOfficial: Boolean,
      userName: String,
      roomName: String,
      debate: StateSnapshot[DebateState],
      disconnect: Callback
  ) = {

    def canAssumeRole(role: Role) = debate.value.debate.forall(
      _.setup.canAssumeRole(userName, role)
    )

    def tryAssumingRole(role: Role): Callback = {
      if (canAssumeRole(role)) {
        debate.modState(_.addParticipant(ParticipantId(userName, role)))
      } else Callback.empty
    }
    def facilitatorsDiv = {
      val facilitators = debate.value.participants.collect {
        case ParticipantId(name, Facilitator) => name
      }
      val isCurrent = facilitators.contains(userName)

      <.div(
        S.optionBox,
        S.simpleSelectable.when(canAssumeRole(Facilitator)),
        S.simpleSelected.when(isCurrent)
      )(
        <.div(S.optionTitle)("Facilitators"),
        commaSeparatedSpans(facilitators.toList.sorted).toVdomArray,
        ^.onClick --> tryAssumingRole(Facilitator)
      )
    }
    def observersDiv = {
      val observers = debate.value.participants.collect {
        case ParticipantId(name, Observer) => name
      }
      val isCurrent = observers.contains(userName)
      <.div(
        S.optionBox,
        S.simpleSelectable.when(canAssumeRole(Observer)),
        S.simpleSelected.when(isCurrent)
      )(
        <.div(S.optionTitle)("Observers"),
        commaSeparatedSpans(observers.toList.sorted).toVdomArray,
        ^.onClick --> tryAssumingRole(Observer)
      )
    }

    val roomPrefix = if (isOfficial) "Official" else "Practice"

    <.div(c"row", S.spaceySubcontainer)(
      <.div(
        <.div(<.strong("Name: "), userName),
        <.div(<.strong(s"$roomPrefix Room: "), roomName)
      ),
      facilitatorsDiv,
      observersDiv,
      <.button(c"btn", S.simpleSelectable)(
        "Disconnect",
        ^.onClick --> disconnect
      )
    )
  }

  class Backend(@unused scope: BackendScope[Unit, Unit]) {

    /** Main render method. */
    def render(@unused props: Unit, @unused state: Unit) = {
      <.div(S.app)(
        LocalLobby.make(Lobby.init) { lobby =>
          MainWebSocket.make(
            mainWebsocketUri,
            onOpen = _ => Callback(println("Main socket opened.")),
            onMessage = (_, msg) => lobby.setState(msg)
          ) {
            case MainWebSocket.Disconnected(_, reason) =>
              <.div(S.loading)(
                """You've been disconnected. This is probably either because of a bug or
                    because the server is restarting. Please refresh the page.
                    Sorry about that.
                """ + reason
              )
            case MainWebSocket.Connecting =>
              <.div(S.loading)("Connecting to metadata server...")
            case MainWebSocket.Connected(sendToMainChannel, _) =>
              LocalConnectionSpecOpt.make(None) { connectionSpecOpt =>
                connectionSpecOpt.value match {
                  case None =>
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

                    LocalString.make(initialValue =
                      getCookie(profileCookieId).getOrElse("")
                    ) { userName =>
                      <.div(S.lobbyContainer, S.spaceyContainer)(
                        <.div(c"form-group row")(
                          <.label(c"col-sm-2 col-form-label")("Profile:"),
                          V.Select.String.modFull(
                            TagMod(c"col-sm-10", S.customSelect)
                          )(
                            choices =
                              noProfileString +: lobby.value.trackedDebaters.toList.sorted,
                            curChoice =
                              if (
                                lobby.value.trackedDebaters
                                  .contains(userName.value)
                              ) {
                                userName.value
                              } else noProfileString,
                            setChoice = (name: String) => {
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
                          )
                        ),
                        V.LiveTextField.String.mod(
                          span = TagMod(c"form-group row", Styles.adminOnly),
                          label = c"col-sm-2 col-form-label",
                          input = c"col-sm-10 form-control"
                        )(userName, labelOpt = Some("Name: ")),
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
                        },
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
                        },
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
                                      ^.onKeyDown ==> (
                                        (e: ReactKeyboardEvent) =>
                                          if (
                                            e.keyCode == dom.ext.KeyCode.Enter
                                          ) enter
                                          else Callback.empty
                                      )
                                    )
                                  )(
                                    roomNameLive,
                                    placeholderOpt = Some("Room")
                                  ),
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
                                ).when(lobbyTab.value != MyDebates),
                                <.div("No rooms to show.")
                                  .when(currentRooms.isEmpty),
                                currentRooms.toVdomArray {
                                  case RoomMetadata(
                                        roomName,
                                        assignedParticipants,
                                        currentParticipants,
                                        status
                                      ) =>
                                    val canEnterRoom =
                                      userName.value.nonEmpty && !currentParticipants
                                        .contains(userName.value)
                                    val statusStyle = {
                                      import RoomStatus._
                                      status match {
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
                                        roomName,
                                        " ",
                                        <.span(statusStyle)(s"($status)")
                                      ),
                                      <.div(
                                        <.strong("Assigned: "),
                                        commaSeparatedSpans(
                                          assignedParticipants.toList.sorted
                                        ).toVdomArray
                                      ).when(assignedParticipants.nonEmpty),
                                      <.div(
                                        <.strong("Present: "),
                                        commaSeparatedSpans(
                                          currentParticipants.toList.sorted
                                        ).toVdomArray
                                      ).when(currentParticipants.nonEmpty),
                                      <.button(
                                        c"btn btn-block btn-danger",
                                        S.adminOnly
                                      )(
                                        "Delete room",
                                        ^.onClick ==> ((e: ReactMouseEvent) => {
                                          e.stopPropagation();
                                          sendToMainChannel(
                                            DeleteRoom(isOfficial, roomName)
                                          )
                                        })
                                      ),
                                      (^.onClick --> enterRoom(
                                        isOfficial,
                                        roomName,
                                        userName.value
                                      )).when(canEnterRoom)
                                    )
                                }
                              )
                            }
                          )
                        }
                      )
                    }
                  case Some(ConnectionSpec(isOfficial, roomName, userName)) =>
                    SyncedDebate.make(
                      getDebateWebsocketUri(isOfficial, roomName, userName),
                      didUpdate = (prevDebate, curDebate) => {
                        curDebate.participants
                          .find(_.name == userName)
                          .map(_.role)
                          .fold(Callback.empty) { role =>
                            def getRoles(debate: DebateState) =
                              debate.debate.foldMap(
                                _.currentTurn.foldMap(_.rolesRemaining)
                              )
                            val newRoles =
                              getRoles(curDebate) -- getRoles(prevDebate)
                            if (newRoles.contains(role)) {
                              Callback {
                                val n = new dom.experimental.Notification(
                                  s"It's your turn as $role in $roomName!"
                                )
                                scalajs.js.timers.setTimeout(7000)(n.close())
                              }
                            } else Callback.empty
                          }
                      }
                    ) {
                      case SyncedDebate.Disconnected(_, reason) =>
                        <.div(S.loading)(
                          """You've been disconnected. This is either due to a bug or the server
                              restarting. Please refresh the page. Sorry about that.
                          """ + reason
                        )
                      case SyncedDebate.Connecting =>
                        <.div(S.loading)("Connecting to debate data server...")
                      case SyncedDebate.Waiting(_) =>
                        <.div(S.loading)("Waiting for debate data...")
                      case SyncedDebate.Connected(debateState, sendUpdate) =>
                        val userId =
                          debateState.value.participants.find(
                            _.name == userName
                          )

                        // looks really bad to use the others haha.
                        // Might have to think through colors (or just do a redesign)
                        val backgroundStyle = S.observerBg

                        <.div(S.debateContainer, S.spaceyContainer)(
                          headerRow(
                            isOfficial,
                            userName,
                            roomName,
                            debateState,
                            disconnect = connectionSpecOpt.setState(None)
                          ),
                          // userInfoRow(roomName, userName, userId),
                          debateState.value.debate match {
                            case None =>
                              userId.map(_.role) match {
                                case Some(Facilitator) =>
                                  facilitatorPanel(
                                    mustAssignRoles = isOfficial,
                                    profiles = lobby.value.trackedDebaters,
                                    qualityService = qualityStoryService,
                                    sendUpdate = sendUpdate
                                  )
                                case _ =>
                                  <.div(S.debateColumn)(
                                    "Waiting for a facilitator to set up the debate."
                                  )
                              }
                            case Some(debate) =>
                              val setup = debate.setup
                              def tryAssumingRole(role: Role): Callback = {
                                if (!setup.canAssumeRole(userName, role))
                                  Callback.empty
                                else {
                                  debateState.modState(
                                    _.addParticipant(
                                      ParticipantId(userName, role)
                                    )
                                  )
                                }
                              }
                              val isCurrentJudge = userId
                                .map(_.role)
                                .collect { case Judge => true }
                                .nonEmpty
                              val questionBoxStyle =
                                if (isCurrentJudge) S.questionBoxCurrent
                                else S.questionBox

                              def showRoleNames(role: DebateRole) = {
                                setup.roles.get(role) match {
                                  case Some(name) =>
                                    if (
                                      !debateState.value.participants
                                        .contains(ParticipantId(name, role))
                                    ) {
                                      <.span(S.missingParticipant)(
                                        name + " (not here)"
                                      )
                                    } else <.span(name)
                                  case None =>
                                    debateState.value.participants.collect {
                                      case ParticipantId(name, `role`) =>
                                        <.span(name)
                                    }.toVdomArray
                                }
                              }

                              <.div(S.debateColumn, backgroundStyle)(
                                <.div(
                                  questionBoxStyle,
                                  S.simpleSelectable.when(
                                    setup.canAssumeRole(userName, Judge)
                                  )
                                )(
                                  <.div(S.questionTitle)(
                                    <.span(S.questionLabel)("Question: "),
                                    setup.question
                                  ),
                                  <.div(S.judgesList)(
                                    "Judge: ",
                                    showRoleNames(Judge)
                                  ),
                                  ^.onClick --> tryAssumingRole(Judge)
                                ),
                                <.div(S.answerBoxesRow, S.spaceySubcontainer)(
                                  setup.answers.zipWithIndex.toVdomArray {
                                    case (answer, answerIndex) =>
                                      val isCurrent = userId
                                        .map(_.role)
                                        .collect {
                                          case Debater(`answerIndex`) =>
                                            true
                                        }
                                        .nonEmpty
                                      val answerBoxStyle =
                                        if (isCurrent) S.answerBoxCurrent
                                        else S.answerBox
                                      <.div(
                                        answerBoxStyle(answerIndex),
                                        S.simpleSelectable.when(
                                          setup.canAssumeRole(
                                            userName,
                                            Debater(answerIndex)
                                          )
                                        )
                                      )(
                                        <.div(S.answerTitle)(
                                          s"${answerLetter(answerIndex)}. $answer"
                                        ),
                                        <.div(S.debatersList)(
                                          "Debater: ",
                                          showRoleNames(Debater(answerIndex))
                                        ),
                                        ^.onClick --> tryAssumingRole(
                                          Debater(answerIndex)
                                        )
                                      )
                                  }
                                ),
                                debatePanel(
                                  roomName,
                                  userId,
                                  setup,
                                  debate,
                                  (d: Debate) =>
                                    debateState
                                      .zoomStateL(DebateState.debate)
                                      .setState(Some(d))
                                )
                              )
                          }
                        )
                    }
                }
              }
          }
        }
      )
    }
  }

  val Component = ScalaComponent
    .builder[Unit]("Full UI")
    .initialState(())
    .renderBackend[Backend]
    .build

  def setupUI(): Unit = {
    Styles.addToDocument()
    Component().renderIntoDOM(
      org.scalajs.dom.document.getElementById("contents")
    )
  }

  final def main(args: Array[String]): Unit = jQuery { () =>
    // get permission for notifications. TODO: only add this in when I'm ready to actually use notifications
    dom.experimental.Notification.requestPermission(result =>
      dom.console.log(result)
    )
    setupUI()
  }
}
