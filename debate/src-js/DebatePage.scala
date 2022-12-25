package debate

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import monocle.std.{all => StdOptics}
import org.scalajs.dom
import scalacss.ScalaCssReact._

import jjm.ui.Mounting

import debate.util._

object DebatePage {
  import Helpers.ClassSetInterpolator
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
    s"${Helpers.wsProtocol}//${dom.document.location.hostname}:8080/$prefix-ws/$roomName?name=$participantId"
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
      val prevRoleOpt = prevDebateOpt.flatMap(_.participants.find(_.name == userName).map(_.role))
      val curRoleOpt  = curDebate.participants.find(_.name == userName).map(_.role)

      val shouldScroll =
        prevDebateOpt.fold(true)(prevDebate =>
          prevDebate.debate.foldMap(DebatePanel.visibleRounds(prevRoleOpt, _)) !=
            curDebate.debate.foldMap(DebatePanel.visibleRounds(curRoleOpt, _))
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
      .find(_.name == userName)
      .map(_.role)
      .fold(Callback.empty) { role =>
        def getRoles(debate: DebateState) = debate
          .debate
          .foldMap(_.currentTransitions.foldMap(_.currentSpeakers))
        val newRoles = getRoles(curDebate) -- prevDebate.foldMap(getRoles)
        if (newRoles.contains(role)) {
          Callback {
            val n = new dom.experimental.Notification(s"It's your turn as $role in $roomName!")
            scalajs.js.timers.setTimeout(7000)(n.close())
          }
        } else
          Callback.empty
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
            val userId = debateState.value.participants.find(_.name == userName)

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

              def canAssumeRole(role: Role) = debate
                .value
                .debate
                .forall(_.setup.canAssumeRole(userName, role))

              def tryAssumingRole(role: Role): Callback =
                if (canAssumeRole(role)) {
                  debate.modState(_.addParticipant(ParticipantId(userName, role)))
                } else
                  Callback.empty
              def facilitatorsDiv = {
                val facilitators = debate
                  .value
                  .participants
                  .collect { case ParticipantId(name, Facilitator) =>
                    name
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
                val observers = debate
                  .value
                  .participants
                  .collect { case ParticipantId(name, Observer) =>
                    name
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

              val roomPrefix =
                if (isOfficial)
                  "Official"
                else
                  "Practice"

              <.div(c"row", S.spaceySubcontainer)(
                <.div(
                  <.div(<.strong("Name: "), userName),
                  <.div(<.strong(s"$roomPrefix Room: "), roomName)
                ),
                facilitatorsDiv,
                observersDiv,
                <.button(c"btn", S.simpleSelectable)("Disconnect", ^.onClick --> disconnect)
              )
            }

            <.div(S.debateContainer, S.spaceyContainer)(
              headerRow(isOfficial, userName, roomName, debateState, disconnect = disconnect),
              debateState.value.debate match {
                case None =>
                  userId.map(_.role) match {
                    case Some(Facilitator) =>
                      <.div(S.debateColumn)(
                        "Debates are no longer set up within the debate room. ",
                        "Please try the 'Create Debates' tab in the lobby."
                      )
                    case _ =>
                      <.div(S.debateColumn)("Waiting for a facilitator to set up the debate.")
                  }
                case Some(debate) =>
                  val setup = debate.setup
                  def tryAssumingRole(role: Role): Callback =
                    if (!setup.canAssumeRole(userName, role))
                      Callback.empty
                    else {
                      debateState.modState(_.addParticipant(ParticipantId(userName, role)))
                    }
                  val isCurrentJudge =
                    userId
                      .map(_.role)
                      .collect { case Judge =>
                        true
                      }
                      .nonEmpty
                  val questionBoxStyle =
                    if (isCurrentJudge)
                      S.questionBoxCurrent
                    else
                      S.questionBox

                  def showRoleNames(role: DebateRole) =
                    setup.roles.get(role) match {
                      case Some(name) =>
                        val nameDisplay =
                          userId.map(_.role) match {
                            case Some(Facilitator) =>
                              V.Select
                                .String
                                .mod(select = TagMod(S.customSelect))(
                                  choices = profiles.toList.sorted,
                                  debateState
                                    .zoomStateO(
                                      DebateState
                                        .debate
                                        .composePrism(StdOptics.some)
                                        .composeLens(Debate.setup)
                                        .composeLens(DebateSetup.roles)
                                        .composeOptional(Optics.index(role))
                                    )
                                    .get
                                )
                            case _ =>
                              <.span(name)
                          }

                        if (!debateState.value.participants.contains(ParticipantId(name, role))) {
                          <.span(S.missingParticipant)(nameDisplay, " (not here)")
                        } else
                          nameDisplay
                      case None =>
                        Helpers
                          .commaSeparatedSpans(
                            debateState
                              .value
                              .participants
                              .collect { case ParticipantId(name, `role`) =>
                                name
                              }
                              .toVector
                          )
                          .toVdomArray
                    }

                  <.div(S.debateColumn, backgroundStyle)(
                    <.div(
                      questionBoxStyle,
                      S.simpleSelectable.when(setup.canAssumeRole(userName, Judge))
                    )(
                      <.div(S.questionTitle)(<.span(S.questionLabel)("Question: "), setup.question),
                      <.div(S.judgesList)("Judge: ", showRoleNames(Judge)),
                      ^.onClick --> tryAssumingRole(Judge)
                    ),
                    <.div(S.answerBoxesRow, S.spaceySubcontainer)(
                      setup
                        .answers
                        .zipWithIndex
                        .toVdomArray { case (answer, answerIndex) =>
                          val isCurrent =
                            userId
                              .map(_.role)
                              .collect { case Debater(`answerIndex`) =>
                                true
                              }
                              .nonEmpty
                          val answerBoxStyle =
                            if (isCurrent)
                              S.answerBoxCurrent
                            else
                              S.answerBox
                          <.div(
                            ^.key := s"answer-$answerIndex",
                            answerBoxStyle(answerIndex),
                            S.simpleSelectable
                              .when(setup.canAssumeRole(userName, Debater(answerIndex)))
                          )(
                            <.div(S.answerTitle)(s"${answerLetter(answerIndex)}. $answer"),
                            <.div(S.debatersList)("Debater: ", showRoleNames(Debater(answerIndex))),
                            ^.onClick --> tryAssumingRole(Debater(answerIndex))
                          )
                        }
                    ),
                    DebatePanel(
                      roomName,
                      userId,
                      debate,
                      (d: Debate) => debateState.zoomStateL(DebateState.debate).setState(Some(d))
                    )
                  )
              }
            )
        }
      }
      .componentDidMount($ =>
        Callback(dom.window.document.title = Helpers.makePageTitle($.props.connectionSpec.roomName))
      )
      .build

  def make(profiles: Set[String], connectionSpec: ConnectionSpec, disconnect: Callback) = Component(
    Props(profiles, connectionSpec, disconnect)
  )
}
