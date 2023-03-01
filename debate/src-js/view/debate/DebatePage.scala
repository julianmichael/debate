package debate
package view.debate

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import org.scalajs.dom
import scalacss.ScalaCssReact._

import jjm.ui.Mounting

import debate.Utils.ClassSetInterpolator
import org.scalajs.jquery.jQuery
// import debate.facades.jQuery
import debate.util._

object DebatePage {
  val S = Styles
  val V = new jjm.ui.View(S)
  val SyncedDebate = SyncedState.forJsonString[DebateStateUpdateRequest, DebateState, DebateState](
    getRequestFromState = DebateStateUpdateRequest.State(_),
    getStateUpdateFromResponse = responseState => _ => responseState
  )

  def renderDebateParticipant(
    anonymize: Boolean,
    userRole: Role,
    userName: String,
    participantRole: Role,
    participantName: String
  ): String =
    if (!anonymize || userRole.canSeeDebaterNames || userName == participantName) {
      s"$participantRole ($participantName)"
    } else
      participantRole.toString

  private def getDebateWebsocketUri(
    isOfficial: Boolean,
    roomName: String,
    participantId: String
  ): String = {
    val prefix =
      if (isOfficial)
        "official"
      else
        "practice"
    s"${Utils.wsProtocol}//${dom.document.location.host}/$prefix-ws/$roomName?name=$participantId"
  }

  private val scrollDebateToBottom = Callback {
    val newSpeechesJQ  = jQuery("#speeches")
    val newSpeechesDiv = newSpeechesJQ(0)
    newSpeechesJQ.scrollTop(newSpeechesDiv.scrollHeight - newSpeechesDiv.clientHeight)
  }
  private def maybeScrollDebateToBottom(
    userName: String,
    prevDebateOpt: Option[DebateState],
    curDebate: DebateState
  ) =
    CallbackTo {
      val prevRole = prevDebateOpt.flatMap(_.participants.get(userName)).getOrElse(Observer)
      val curRole  = curDebate.participants.get(userName).getOrElse(Observer)

      val shouldScroll =
        prevDebateOpt.fold(true)(prevDebate =>
          DebatePanel.visibleRounds(prevRole, prevDebate.debate) !=
            DebatePanel.visibleRounds(curRole, curDebate.debate)
        )
      if (!shouldScroll || jQuery("#speeches").length < 1)
        Callback.empty
      else {
        val speechesJQ  = jQuery("#speeches")
        val speechesDiv = speechesJQ(0)
        val isScrolledToBottom =
          speechesDiv.scrollHeight - speechesJQ.scrollTop() - speechesJQ.outerHeight() < 1
        if (!isScrolledToBottom)
          scrollDebateToBottom
        else
          Callback.empty
      }
    }.flatten

  private def maybeSendTurnNotification(
    userName: String,
    roomName: String,
    prevDebate: Option[DebateState],
    curDebate: DebateState
  ): Callback =
    curDebate
      .participants
      .get(userName)
      .flatMap(_.asLiveDebateRoleOpt)
      .fold(Callback.empty) { role =>
        def getRoles(debate: DebateState) = debate.debate.currentTransitions.currentSpeakers
        val newRoles                      = getRoles(curDebate) -- prevDebate.foldMap(getRoles)
        if (newRoles.contains(role)) {
          Callback {
            val n = new dom.experimental.Notification(s"It's your turn as $role in $roomName!")
            scalajs.js.timers.setTimeout(7000)(n.close())
          }
        } else
          Callback.empty
      }

  /** Top row showing debate metadata and observers.  */
  private def headerRow(
    userName: String,
    userRole: Role,
    isOfficial: Boolean,
    roomName: String,
    debate: StateSnapshot[DebateState],
    disconnect: Callback
  ) = {

    def canAssumeRole(role: Role) = debate.value.canSwitchToRole(userName, role)

    def tryAssumingRole(role: Role): Callback =
      if (canAssumeRole(role)) {
        debate.modState(_.addParticipant(userName, role))
      } else
        Callback.empty

    val roomPrefix =
      if (isOfficial)
        "Official"
      else
        "Practice"

    <.div(c"row", S.spaceySubcontainer, ^.alignItems.center)(
      <.button(c"btn btn-sm", S.simpleSelectable, ^.fontSize.small)(
        <.i(c"bi bi-arrow-left"),
        " Exit",
        ^.onClick --> disconnect
      ),
      <.div(<.strong(s"$roomPrefix Room: "), roomName),
      <.div(<.strong("Name: "), userName),
      <.div(<.strong(s"Role: "), userRole.toString),
      <.div(
          <.strong(
            S.simpleSelectableText.when(canAssumeRole(Observer)),
            s"Observers:",
            ^.onClick --> tryAssumingRole(Observer)
          ),
          " ", {
            val observers = debate
              .value
              .participants
              .view
              .collect { case (name, role @ (Observer | Facilitator)) =>
                name -> (role == Facilitator)
              }
              .toVector
              .sortBy(_._1)

            if (observers.isEmpty)
              <.span(S.veryGreyedOut)("none")
            else
              Utils
                .delimitedTags[Vector, (String, Boolean)](
                  observers,
                  getTag = { case (name, isAdmin) =>
                    val roleToToggleTo =
                      if (isAdmin)
                        Observer
                      else
                        Facilitator
                    <.span(S.facilitatorName.when(isAdmin))(
                      name,
                      (^.onClick --> (tryAssumingRole(roleToToggleTo))).when(name == userName)
                    )
                  }
                )
                .toVdomArray
          }
        )
        .when(userRole.canSeeDebaterNames || userRole == Observer),
      <.div(S.grow),
      <.div(<.strong("Rules: "), debate.value.debate.setup.rules.summary)
    )
  }

  private def showRoleNames(
    anonymize: Boolean,
    debateState: StateSnapshot[DebateState],
    profiles: Set[String],
    userRole: Role,
    userName: String,
    role: LiveDebateRole
  ) =
    debateState.value.debate.setup.roles.get(role) match {
      case Some(name) =>
        userRole match {
          case Facilitator =>
            <.span(
              s"$role: ",
              V.Select
                .String
                .mod(select = TagMod(S.customSelect, ^.width.auto))(
                  choices = profiles.toList.sorted,
                  debateState
                    .zoomStateO(
                      DebateState
                        .debate
                        .composeLens(Debate.setup)
                        .composeLens(DebateSetup.roles)
                        .composeOptional(Optics.index(role))
                    )
                    .get
                )
            )
          case _ =>
            val nameDisplay = renderDebateParticipant(anonymize, userRole, userName, role, name)
            if (userRole == role) {
              <.span(S.presentParticipant)(nameDisplay)
            } else if (debateState.value.participants.get(name).isEmpty) {
              <.span(S.missingParticipant)(nameDisplay, " (not here)")
            } else
              <.span(S.presentParticipant)(nameDisplay, " (present)")
        }

      case None =>
        Utils
          .delimitedSpans(
            debateState
              .value
              .participants
              .collect { case (name, `role`) =>
                name
              }
              .toVector
          )
          .toVdomArray
    }

  private def correctAnswerRadio(answerIndex: Int, correctAnswerIndex: StateSnapshot[Int]) =
    ReactFragment(
      <.input(S.correctAnswerRadio)(
        ^.`type`  := "radio",
        ^.name    := "correctAnswerIndex",
        ^.value   := answerIndex,
        ^.checked := correctAnswerIndex.value == answerIndex,
        ^.onChange --> correctAnswerIndex.setState(answerIndex),
        ^.onClick ==> (e => Callback(e.stopPropagation()))
      ),
      <.span(c"mr-2", S.inputRowItem)(
        <.span(S.correctAnswerLabel)("Correct"),
        S.hidden.when(correctAnswerIndex.value != answerIndex)
      )
    )

  private def qaAndRolesRow(
    isOfficial: Boolean,
    anonymize: Boolean,
    profiles: Set[String],
    userName: String,
    role: Role,
    debateState: StateSnapshot[DebateState]
  ) = {
    def canAssumeRole(role: Role) = !isOfficial && debateState.value.canSwitchToRole(userName, role)
    def tryAssumingRole(role: Role): Callback =
      if (!canAssumeRole(role))
        Callback.empty
      else {
        debateState.modState(_.addParticipant(userName, role))
      }
    <.div(S.row)(
      <.div(S.grow, S.col, S.debateHeaderRowCol)( // question and answers
        <.div(
          S.row,
          if (role == Judge)
            S.questionBoxCurrent
          else
            S.questionBox,
          S.simpleSelectable.when(canAssumeRole(Judge))
        )(
          <.div(S.grow)(
            <.span(S.questionTitle)("Question: "),
            debateState.value.debate.setup.question
          ),
          " ",
          <.div(S.judgesList)(
            showRoleNames(anonymize, debateState, profiles, role, userName, Judge)
          ),
          ^.onClick --> tryAssumingRole(Judge)
        ),
        debateState
          .value
          .debate
          .setup
          .answers
          .zipWithIndex
          .toVdomArray { case (answer, answerIndex) =>
            <.div(S.row)(
              ^.key := s"answer-$answerIndex",
              if (role == Debater(answerIndex))
                S.answerBoxCurrent(answerIndex)
              else
                S.answerBox(answerIndex),
              S.simpleSelectable.when(canAssumeRole(Debater(answerIndex)))
            )(
              <.div(
                  correctAnswerRadio(
                    answerIndex,
                    debateState.zoomStateL(
                      DebateState
                        .debate
                        .composeLens(Debate.setup)
                        .composeLens(DebateSetup.correctAnswerIndex)
                    )
                  )
                )
                .when(role == Facilitator),
              <.div(S.grow)(<.span(S.answerTitle)(s"${answerLetter(answerIndex)}. "), answer),
              " ",
              <.div(S.debatersList)(
                showRoleNames(
                  anonymize,
                  debateState,
                  profiles,
                  role,
                  userName,
                  Debater(answerIndex)
                )
              ),
              ^.onClick --> tryAssumingRole(Debater(answerIndex))
            )
          },
        <.div(S.col, S.offlineJudgesBox)(
            <.h5("Offline judges"),
            debate
              .view
              .lobby
              .DebateCreationPanel
              .offlineJudgesConfig(
                profiles,
                debateState.zoomStateL(
                  DebateState
                    .debate
                    .composeLens(Debate.setup)
                    .composeLens(DebateSetup.offlineJudges)
                )
              )
          )
          .when(role == Facilitator)
      )
    )
  }

  def apply(
    profiles: Set[String],
    connectionSpec: ConnectionSpec,
    disconnect: Callback,
    sendToMainChannel: MainChannelRequest => Callback
  ) =
    Mounting
      .make(Callback(dom.window.document.title = Utils.makePageTitle(connectionSpec.roomName))) {
        val isOfficial = connectionSpec.isOfficial
        val roomName   = connectionSpec.roomName
        val userName   = connectionSpec.participantName
        SyncedDebate.make(
          getDebateWebsocketUri(isOfficial, roomName, userName),
          didUpdate =
            (prevDebate, curDebate) =>
              maybeSendTurnNotification(userName, roomName, prevDebate, curDebate) >>
                maybeScrollDebateToBottom(userName, prevDebate, curDebate)
        ) {
          case SyncedDebate.Disconnected(reconnect, reason) =>
            Mounting.make(AsyncCallback.unit.delayMs(5000).completeWith(_ => reconnect))(
              <.div(S.loading)(
                """You've been disconnected. Will attempt to reconnect every 5 seconds.
                    If you don't reconnect after a few seconds,
                    Please refresh the page. """ + reason
              )
            )
          case SyncedDebate.Connecting =>
            <.div(S.loading)("Connecting to debate data server...")
          case SyncedDebate.Connected(_, None) =>
            <.div(S.loading)("Waiting for debate data...")
          case SyncedDebate.Connected(_, Some(debateState)) =>
            val role = debateState.value.participants.get(userName).getOrElse(Observer)

            // looks really bad to use the others haha.
            // Might have to think through colors (or just do a redesign)
            val backgroundStyle = S.observerBg

            val anonymize =
              isOfficial &&
                (!debateState.value.debate.isOver ||
                  (role.asDebateRoleOpt.nonEmpty &&
                    debateState.value.debate.feedback.get(userName).isEmpty))

            <.div(S.debateContainer, S.spaceyContainer)(
              headerRow(userName, role, isOfficial, roomName, debateState, disconnect = disconnect),
              if (role == Peeper) {
                <.div("No peeping! Go judge your assigned debates for this story first.")
              } else if (role == OfflineJudge && !debateState.value.debate.isOver) {
                <.div("This debate isn't over yet! Come back later to judge it.")
              } else {
                <.div(S.debateColumn, S.spaceyContainer, backgroundStyle)(
                  qaAndRolesRow(
                    isOfficial = isOfficial,
                    anonymize = anonymize,
                    profiles = profiles,
                    userName = userName,
                    role = role,
                    debateState = debateState
                  ),
                  DebatePanel(
                    profiles = profiles,
                    roomName = roomName,
                    userName = userName,
                    role = role,
                    debate = debateState.zoomStateL(DebateState.debate),
                    anonymize = anonymize,
                    sendToMainChannel = sendToMainChannel
                  )
                )
              }
            )
        }
      }

}
