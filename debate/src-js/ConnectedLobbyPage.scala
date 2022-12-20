package debate

import org.scalajs.dom

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.MonocleReact._

import scalacss.ScalaCssReact._

import cats.~>
import cats.implicits._

import debate.util._
import scala.concurrent.Future
import jjm.io.HttpUtil

object ConnectedLobbyPage {
  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._
  import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)
  val SyncedDebate = SyncedState
    .forJsonString[DebateStateUpdateRequest, DebateState, DebateState](
      getRequestFromState = DebateStateUpdateRequest.State(_),
      getStateUpdateFromResponse = responseState => _ => responseState
    )

  val facilitatorPanel = new FacilitatorPanel(S, V)

  def getDebateWebsocketUri(
      isOfficial: Boolean,
      roomName: String,
      participantId: String
  ): String = {
    val prefix = if (isOfficial) "official" else "practice"
    s"${Helpers.wsProtocol()}//${dom.document.location.host}/$prefix-ws/$roomName?name=$participantId"
  }

  val httpProtocol = dom.document.location.protocol
  val qualityApiUrl: String = {
    s"$httpProtocol//${dom.document.location.host}/$qualityServiceApiEndpoint"
  }
  type DelayedFuture[A] = () => Future[A]
  val toAsyncCallback = {
    λ[DelayedFuture ~> AsyncCallback](f => AsyncCallback.fromFuture(f()))
  }
  val qualityStoryService = quality.QuALITYService(
    HttpUtil
      .makeHttpPostClient[quality.QuALITYService.Request](
        qualityApiUrl
      )
      .andThenK(toAsyncCallback)
  )

  def make(
      lobby: StateSnapshot[Lobby],
      connectionSpec: ConnectionSpec,
      disconnect: Callback
  ) = {
    val isOfficial = connectionSpec.isOfficial
    val roomName = connectionSpec.roomName
    val userName = connectionSpec.participantName
    SyncedDebate.make(
      getDebateWebsocketUri(isOfficial, roomName, userName),
      didUpdate = (prevDebate, curDebate) => {
        curDebate.participants
          .find(_.name == userName)
          .map(_.role)
          .fold(Callback.empty) { role =>
            def getRoles(debate: DebateState) =
              debate.debate.foldMap(
                _.currentTransitions.foldMap(_.currentSpeakers)
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

        /** Top row showing non-debating roles (user can click one to change
          * roles).
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
              Helpers
                .commaSeparatedSpans(facilitators.toList.sorted)
                .toVdomArray,
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

        <.div(S.debateContainer, S.spaceyContainer)(
          headerRow(
            isOfficial,
            userName,
            roomName,
            debateState,
            disconnect = disconnect
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
                        .collect { case Debater(`answerIndex`) =>
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
                DebatePanel(
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
