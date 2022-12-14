package debate

import annotation.unused

import org.scalajs.dom

import org.scalajs.jquery.jQuery

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.MonocleReact._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import scala.util.Try

import cats.~>
import cats.implicits._

import debate.util._
import scala.concurrent.Future
import jjm.io.HttpUtil
import jjm.OrWrapped

/** The main webapp. */
object App {
  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  implicit class ClassSetInterpolator(val sc: StringContext) extends AnyVal {
    def c(args: Any*) = {
      // concatenate everything: use the built-in S method (which happens to be used in the S interpolator)
      ^.classSet1(sc.s(args: _*))
    }
  }

  val wsProtocol = {
    if (dom.document.location.protocol == "https:") "wss:" else "ws:"
  }

  val DebateWebSocket =
    WebSocketConnection2.forJsonString[DebateState, DebateState]
  val SyncedDebate = SyncedState
    .forJsonString[DebateStateUpdateRequest, DebateState, DebateState](
      getRequestFromState = DebateStateUpdateRequest.State(_),
      getStateUpdateFromResponse = responseState => _ => responseState
    )
  def getDebateWebsocketUri(
      isOfficial: Boolean,
      roomName: String,
      participantId: String
  ): String = {
    val prefix = if (isOfficial) "official" else "practice"
    s"$wsProtocol//${dom.document.location.host}/$prefix-ws/$roomName?name=$participantId"
  }

  val MainWebSocket =
    WebSocketConnection2.forJsonString[MainChannelRequest, Lobby]
  val mainWebsocketUri: String = {
    s"$wsProtocol//${dom.document.location.host}/main-ws"
  }

  val httpProtocol = dom.document.location.protocol
  val qualityApiUrl: String = {
    s"$httpProtocol//${dom.document.location.host}/$qualityServiceApiEndpoint"
  }

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

  val defaultRoomName: String =
    jQuery("#defaultRoomName").attr("value").toOption.getOrElse("")

  // Shortcuts for styles and view elements

  val S = Styles
  val V = new jjm.ui.View(S)

  // instantiate the HOCs we need

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
        Helpers.commaSeparatedSpans(facilitators.toList.sorted).toVdomArray,
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
        Helpers.commaSeparatedSpans(observers.toList.sorted).toVdomArray,
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
            onMessage = (_, msg) =>
              msg
                .flatMap(response => lobby.setState(response).asAsyncCallback)
                .toCallback
            // lobby.setState(msg)
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
            case MainWebSocket.Connected(sendToMainChannel) =>
              LocalConnectionSpecOpt.make(None) { connectionSpecOpt =>
                connectionSpecOpt.value match {
                  case None =>
                    Helpers.makeLobbyPageWhenNotConnected(
                      lobby = lobby,
                      sendToMainChannel = sendToMainChannel,
                      connectionSpecOpt = connectionSpecOpt
                    )
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
                              getRoles(curDebate) -- prevDebate
                                .foldMap(getRoles)
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
                      case SyncedDebate.Connected(_, None) =>
                        <.div(S.loading)("Waiting for debate data...")
                      case SyncedDebate
                            .Connected(sendUpdate, Some(debateState)) =>
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
