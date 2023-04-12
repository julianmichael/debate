package debate
package view.lobby

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.quality.QuALITYService

object AdminPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  case class Props(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => Callback,
    connect: ConnectionSpec => Callback,
    userName: String
  )

  def apply(
    qualityService: QuALITYService[AsyncCallback],
    lobby: Lobby,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit],
    connect: ConnectionSpec => Callback,
    userName: String
  ) = Component(Props(qualityService, lobby, sendToMainChannel, connect, userName))

  val Component =
    ScalaComponent
      .builder[Props]("Admin Panel")
      .render_P { case Props(qualityService, lobby, sendToMainChannel, connect, userName) =>
        val joinRoomOpt = Option(userName)
          .filter(_.nonEmpty)
          .map(userName =>
            (isOfficial: Boolean, roomName: String) =>
              connect(ConnectionSpec(isOfficial, roomName, userName))
          )
        val joinOfficialRoomOpt = joinRoomOpt
          .map(joinRoom => (roomName: String) => joinRoom.apply(true, roomName))

        TabNav("admin-tab", initialTabIndex = 1)(
          "Profiles" ->
            TabNav.tab(
              <.div(c"card-body", S.spaceySubcontainer)(
                ProfilesPanel(
                  lobby = lobby,
                  joinOfficialRoomOpt = joinOfficialRoomOpt,
                  sendToMainChannel = sendToMainChannel
                )
              )
            ),
          "Schedule Debates" ->
            TabNav.tab(
              <.div(c"card-body", S.spaceySubcontainer)(
                DebateSchedulingPanel(lobby, sendToMainChannel)
              )
            ),
          "Schedule Extra Judging" ->
            TabNav.tab(
              <.div(c"card-body", S.spaceySubcontainer)(
                JudgingSchedulingPanel(lobby, sendToMainChannel)
              )
            ),
          "Create Debate" ->
            TabNav.tab(
              <.div(c"card-body", S.spaceySubcontainer)(
                DebateCreationPanel.make(
                  lobby = lobby,
                  qualityService = qualityService,
                  registerRuleConfig = sendToMainChannel,
                  removeRuleConfig = sendToMainChannel,
                  joinDebate = joinRoomOpt,
                  initDebate = sendToMainChannel
                )
              )
            )
        )
      }
      .build
}
