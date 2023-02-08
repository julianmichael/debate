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

  def getDebateWebsocketUri(
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

  val scrollDebateToBottom = Callback {
    val newSpeechesJQ  = jQuery("#speeches")
    val newSpeechesDiv = newSpeechesJQ(0)
    newSpeechesJQ.scrollTop(newSpeechesDiv.scrollHeight - newSpeechesDiv.clientHeight)
  }
  def maybeScrollDebateToBottom(
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

  def maybeSendTurnNotification(
    userName: String,
    roomName: String,
    prevDebate: Option[DebateState],
    curDebate: DebateState
  ): Callback =
    curDebate
      .participants
      .get(userName)
      .flatMap(_.asDebateRoleOpt)
      .fold(Callback.empty) { role =>
        def getRoles(debate: DebateState) = debate
          .debate
          .currentTransitions
          .foldMap(_.currentSpeakers)
        val newRoles = getRoles(curDebate) -- prevDebate.foldMap(getRoles)
        if (newRoles.contains(role)) {
          Callback {
            val n = new dom.experimental.Notification(s"It's your turn as $role in $roomName!")
            scalajs.js.timers.setTimeout(7000)(n.close())
          }
        } else
          Callback.empty
      }

  /** Top row showing debate metadata and observers.  */
  def headerRow(
    userName: String,
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
      <.div(<.strong("Name: "), userName),
      <.div(<.strong(s"$roomPrefix Room: "), roomName),
      <.div(S.grow)(
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
      ),
      <.div(<.strong("Rules: "), debate.value.debate.setup.rules.summary)
    )
  }

  def showRoleNames(
    debateState: StateSnapshot[DebateState],
    profiles: Set[String],
    userRole: Role,
    role: DebateRole
  ) =
    debateState.value.debate.setup.roles.get(role) match {
      case Some(name) =>
        val nameDisplay =
          userRole match {
            case Facilitator =>
              V.Select
                .String
                .mod(select = TagMod(S.customSelect))(
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
            case _ =>
              <.span(name)
          }

        if (debateState.value.participants.get(name).isEmpty) {
          <.span(S.missingParticipant)(nameDisplay, " (not here)")
        } else
          nameDisplay
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

  def correctAnswerRadio(answerIndex: Int, correctAnswerIndex: StateSnapshot[Int]) = ReactFragment(
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

  def qaAndRolesRow(
    profiles: Set[String],
    userName: String,
    role: Role,
    debateState: StateSnapshot[DebateState]
  ) = {
    def tryAssumingRole(role: Role): Callback =
      if (!debateState.value.canSwitchToRole(userName, role))
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
          S.simpleSelectable.when(debateState.value.canSwitchToRole(userName, Judge))
        )(
          <.div(S.grow)(
            <.span(S.questionTitle)("Question: "),
            debateState.value.debate.setup.question
          ),
          " ",
          <.div(S.judgesList)(showRoleNames(debateState, profiles, role, Judge)),
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
              S.simpleSelectable
                .when(debateState.value.canSwitchToRole(userName, Debater(answerIndex)))
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
                showRoleNames(debateState, profiles, role, Debater(answerIndex))
              ),
              ^.onClick --> tryAssumingRole(Debater(answerIndex))
            )
          }
      )
    )
  }

  case class Props(profiles: Set[String], connectionSpec: ConnectionSpec, disconnect: Callback)

  val Component =
    ScalaComponent
      .builder[Props]("Debate Page")
      .render_P { case Props(profiles, connectionSpec, disconnect) =>
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

            <.div(S.debateContainer, S.spaceyContainer)(
              headerRow(userName, isOfficial, roomName, debateState, disconnect = disconnect),
              <.div(S.debateColumn, S.spaceyContainer, backgroundStyle)(
                qaAndRolesRow(
                  profiles = profiles,
                  userName = userName,
                  role = role,
                  debateState = debateState
                ),
                DebatePanel(roomName, userName, role, debateState.zoomStateL(DebateState.debate))
              )
            )
        }
      }
      .componentDidMount($ =>
        Callback(dom.window.document.title = Utils.makePageTitle($.props.connectionSpec.roomName))
      )
      .build

  def make(profiles: Set[String], connectionSpec: ConnectionSpec, disconnect: Callback) = Component(
    Props(profiles, connectionSpec, disconnect)
  )
}
