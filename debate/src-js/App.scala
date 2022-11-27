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
          // val tokens = sc.s(args: _*).split(",")
          // Person(tokens(0), tokens(1).toInt)
        ^.classSet1(sc.s(args: _*))
      }
  }

  def commaSeparatedSpans[F[_]: Foldable: Functor](fa: F[String]) = {
    fa.map(x => Vector(<.span(x))).intercalate(Vector(<.span(", ")))
  }

  val wsProtocol = {
    if (dom.document.location.protocol == "https:") "wss" else "ws"
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
  val SyncedDebate = boopickleSyncedState[DebateStateUpdateRequest, DebateState](
    getRequestFromState = DebateStateUpdateRequest.State(_)
  )
  def getDebateWebsocketUri(isScheduled: Boolean, roomName: String, participantId: String): String = {
    val prefix = if(isScheduled) "scheduled" else "open"
    s"$wsProtocol://${dom.document.location.host}/$prefix-ws/$roomName?name=$participantId"
  }

  val MainWebSocket = boopickleWebsocket[MainChannelRequest, Lobby]
  val mainWebsocketUri: String = {
    s"$wsProtocol://${dom.document.location.host}/main-ws"
  }


  val httpProtocol = dom.document.location.protocol
  val qualityApiUrl: String = {
    s"$httpProtocol://${dom.document.location.host}/$qualityStoryApiEndpoint"
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  type DelayedFuture[A] = () => Future[A]
  val toAsyncCallback = {
    λ[DelayedFuture ~> AsyncCallback](f =>
      AsyncCallback.fromFuture(f())
    )
  }
  def wrap[F[_]] = λ[F ~> OrWrapped[F, *]]{f =>
    OrWrapped.wrapped(f)
  }

  // def tapPrint[F[_]: Functor] = λ[F ~> F](x =>
  //   { Functor[F].map(x)(y => { println(y); y}) }
  // )

  val qualityStoryService = HttpUtil.makeHttpPostClient[QuALITYStoryRequest](
    qualityApiUrl
  ).andThenK(toAsyncCallback)//.andThenK(wrap)//.andThenK(tapPrint)

  import jjm.ui.LocalState

  case class ConnectionSpec(
    isScheduled: Boolean,
    roomName: String,
    participantName: String
  )


  sealed trait LobbyTab extends Product with Serializable {
    import LobbyTab._
    override def toString = this match {
      case MyDebates => "My Debates"
      case AllScheduledDebates => "All Scheduled Debates"
      case OpenDebates => "Open Debates"
    }
  }
  object LobbyTab {
    case object MyDebates extends LobbyTab
    case object AllScheduledDebates extends LobbyTab
    case object OpenDebates extends LobbyTab
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
    // Option(x).flatMap(_.nonEmpty).traverse(i => Try(i.toInt).toOption),
    _.foldMap(_.toString)
  )
  // val TurnTypeSpecLocal = new LocalState[DebateRoundTypeSpec]
  // val TurnTypeSpecSelect = V.Select[DebateRoundTypeSpec](_.toString)

  val debatePanel = new DebatePanel(S, V)
  val facilitatorPanel = new FacilitatorPanel(S, V)

  /** Top row showing non-debating roles (user can click one to change roles).
    */
  def headerRow(
      userName: String,
      roomName: String,
      debate: StateSnapshot[DebateState],
      disconnect: Callback
  ) = {

    // val roleOpt = debate.value.participants
    //   .find(_.name == userName)
    //   .map(_.role)

    def assumeRole(role: Role): Callback = {
      debate.modState(_.addParticipant(ParticipantId(userName, role)))
    }
    def facilitatorsDiv = {
      val facilitators = debate.value.participants.collect {
        case ParticipantId(name, Facilitator) => name
      }
      val isCurrent = facilitators.contains(userName)

      <.div(S.optionBox, S.simpleSelectable, S.simpleSelected.when(isCurrent))(
        <.div(S.optionTitle)("Facilitators"),
        commaSeparatedSpans(facilitators.toList.sorted).toVdomArray,
        ^.onClick --> assumeRole(Facilitator)
      )
    }
    def observersDiv = {
      val observers = debate.value.participants.collect {
        case ParticipantId(name, Observer) => name
      }
      val isCurrent = observers.contains(userName)
      <.div(S.optionBox, S.simpleSelectable, S.simpleSelected.when(isCurrent))(
        <.div(S.optionTitle)("Observers"),
        commaSeparatedSpans(observers.toList.sorted).toVdomArray,
        ^.onClick --> assumeRole(Observer)
      )
    }

    <.div(c"row", S.spaceySubcontainer)(
      <.div(
        <.div(<.strong("Name: "), userName),
        <.div(<.strong("Room: "), roomName)
      ),
      facilitatorsDiv,
      observersDiv,
      <.button(c"btn", S.simpleSelectable)(
        "Disconnect", ^.onClick --> disconnect
      )
    )
  }

  class Backend(@unused scope: BackendScope[Unit, Unit]) {

    /** Main render method. */
    def render(@unused props: Unit, @unused state: Unit) = {
      <.div(S.app)(
        LocalConnectionSpecOpt.make(None) { connectionSpecOpt =>
          connectionSpecOpt.value match {
            case None =>
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
                    def enterRoom(isScheduled: Boolean, roomName: String, participantName: String) =
                      connectionSpecOpt.setState(
                        Some(ConnectionSpec(isScheduled, roomName, participantName))
                      )
                    // TODO change page title? maybe do this on mount for the debate room component instead
                    // >> Callback(dom.window.document.title = makePageTitle(roomName)) >>
                    // Callback(dom.window.history.replaceState("", makePageTitle(roomName), roomName))
                    // roomName.setState(roomNameLive.value)

                    val noProfileString = "(select profile)"
                    val profileCookieId = "debate-profile"

                    LocalString.make(initialValue = getCookie(profileCookieId).getOrElse("")) { userName =>
                      <.div(S.lobbyContainer, S.spaceyContainer)(
                        <.div(c"form-group row")(
                          <.label(c"col-sm-2 col-form-label")("Profile:"),
                          V.Select.String.modFull(c"col-sm-10 custom-select")(
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
                              val adjustedName = if(name == noProfileString) "" else name
                              userName.setState(adjustedName) >> Callback(setCookie(profileCookieId, adjustedName, expires = 5))
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
                          val isDisabled = (lobby.value.trackedDebaters + "" + "(no profile)").contains(name)
                          <.button(c"btn btn-primary btn-block")(
                            "Create profile",
                            ^.disabled := isDisabled,
                            (^.onClick --> sendToMainChannel(
                              RegisterDebater(userName.value)
                            )).when(!isDisabled),
                          )
                        },
                        LocalLobbyTab.make(LobbyTab.MyDebates) { lobbyTab =>
                          import LobbyTab._
                          val myDebates = lobby.value.scheduledRooms
                            .filter(_.assignedParticipants.contains(userName.value))
                          val isScheduled = lobbyTab.value match {
                            case OpenDebates => false
                            case _ => true
                          }
                          val currentRooms = lobbyTab.value match {
                            case MyDebates => myDebates
                            case AllScheduledDebates => lobby.value.scheduledRooms
                            case OpenDebates => lobby.value.openRooms
                          }
                          <.div(c"card", ^.textAlign.center)(
                            <.div(c"card-header")(
                              <.ul(c"nav nav-fill nav-tabs card-header-tabs")(
                                List(MyDebates, AllScheduledDebates, OpenDebates).toVdomArray(tab =>
                                  <.li(c"nav-item")(
                                    <.a(^.classSet1("nav-link", "active" -> (tab == lobbyTab.value)))(
                                      ^.href := "#",
                                      ^.onClick --> lobbyTab.setState(tab),
                                      tab.toString,
                                    )
                                  )
                                )
                              )
                            ),
                            LocalString.make("") { roomNameLive =>
                              val canEnter = roomNameLive.value.nonEmpty && userName.value.nonEmpty
                              val enter = if(canEnter) enterRoom(isScheduled, roomNameLive.value, userName.value) else Callback.empty
                              <.div(c"card-body", S.spaceySubcontainer)(
                                <.div(c"input-group", ^.width.auto)(
                                  V.LiveTextField.String.modInput(
                                    input = TagMod(
                                      c"form-control",
                                      ^.onKeyDown ==> ((e: ReactKeyboardEvent) => if(e.keyCode == dom.ext.KeyCode.Enter) enter else Callback.empty)
                                    ))(roomNameLive, placeholderOpt = Some("Room")
                                  ),
                                  <.div(c"input-group-append")(
                                    <.button(c"btn btn-primary")(
                                      if(currentRooms.exists(_.name == roomNameLive.value)) "Join" else "Create",
                                      ^.`type` := "button",
                                      ^.disabled := !canEnter,
                                      ^.onClick --> enter
                                    )
                                  )
                                ).when(lobbyTab.value != MyDebates),
                                <.div("No rooms to show.").when(currentRooms.isEmpty),
                                currentRooms.toVdomArray {
                                  case RoomMetadata(roomName, assignedParticipants, currentParticipants, status) =>
                                    val canEnterRoom =
                                      userName.value.nonEmpty && !currentParticipants
                                        .contains(userName.value)
                                    val statusStyle = {
                                      import RoomStatus._
                                      status match {
                                        case SettingUp  => S.settingUpStatusLabel
                                        case InProgress => S.inProgressStatusLabel
                                        case Complete   => S.completeStatusLabel
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
                                      (^.onClick --> enterRoom(
                                        isScheduled,
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
                }
              }
            case Some(ConnectionSpec(isScheduled, roomName, userName)) =>
              SyncedDebate.make(
                getDebateWebsocketUri(isScheduled, roomName, userName),
                didUpdate = (prevDebate, curDebate) => {
                  curDebate.participants
                    .find(_.name == userName)
                    .map(_.role)
                    .fold(Callback.empty) { role =>
                      def getRoles(debate: DebateState) = debate.debate.foldMap(
                        _.currentTurn.foldMap(_.rolesRemaining)
                      )
                      val newRoles = getRoles(curDebate) -- getRoles(prevDebate)
                      if(newRoles.contains(role)) {
                        Callback {
                          val n = new dom.experimental.Notification(s"It's your turn as $role in $roomName!")
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
                    debateState.value.participants.find(_.name == userName)

                  // val backgroundStyle = userId.map(_.role).fold(S.noRoleBg) {
                  //   case Facilitator => S.facilitatorBg
                  //   case Observer => S.observerBg
                  //   case Judge => S.judgeBg
                  //   case Debater(i) => S.debaterBg(i)
                  // }

                  // looks really bad to use the others haha.
                  // Might have to think through colors (or just do a redesign)
                  val backgroundStyle = S.observerBg

                  <.div(S.debateContainer, S.spaceyContainer)(
                    headerRow(
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
                            facilitatorPanel(qualityStoryService, sendUpdate)
                          case _ =>
                            <.div(S.debateColumn)(
                              "Waiting for a facilitator to set up the debate."
                            )
                        }
                      case Some(debate) =>
                        val setup = debate.setup
                        def assumeRole(role: Role): Callback = {
                          debateState.modState(
                            _.addParticipant(
                              ParticipantId(userName, role)
                            )
                          )
                        }
                        val isCurrentJudge = userId
                          .map(_.role)
                          .collect { case Judge => true }
                          .nonEmpty
                        val questionBoxStyle =
                          if (isCurrentJudge) S.questionBoxCurrent
                          else S.questionBox
                        <.div(S.debateColumn, S.spaceyContainer, backgroundStyle)(
                          <.div(questionBoxStyle)(
                            <.div(S.questionTitle)(
                              <.span(S.questionLabel)("Question: "),
                              setup.question
                            ),
                            <.div(S.judgesList)(
                              "Judges: ",
                              debateState.value.participants.collect {
                                case ParticipantId(name, Judge) =>
                                  <.span(name)
                              }.toVdomArray
                            ),
                            ^.onClick --> assumeRole(Judge)
                          ),
                          <.div(S.answerBoxesRow, S.spaceySubcontainer)(
                            setup.answers.zipWithIndex.toVdomArray {
                              case (answer, answerIndex) =>
                                val isCurrent = userId
                                  .map(_.role)
                                  .collect { case Debater(`answerIndex`) =>
                                    true
                                  }
                                  .nonEmpty
                                val answerBoxStyle =
                                  if (isCurrent) S.answerBoxCurrent
                                  else S.answerBox
                                <.div(answerBoxStyle(answerIndex))(
                                  <.div(S.answerTitle)(
                                    s"${answerLetter(answerIndex)}. $answer"
                                  ),
                                  <.div(S.debatersList)(
                                    "Debaters: ",
                                    debateState.value.participants.collect {
                                      case ParticipantId(
                                            name,
                                            Debater(`answerIndex`)
                                          ) =>
                                        <.span(name)
                                    }.toVdomArray
                                  ),
                                  ^.onClick --> assumeRole(
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
    dom.experimental.Notification.requestPermission(result => dom.console.log(result))
    setupUI()
  }
}
