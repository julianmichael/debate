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

  def apply(
    lobby: Lobby,
    qualityService: QuALITYService[AsyncCallback],
    userName: String,
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => Callback
  ) = {
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
      "Create Debate" ->
        TabNav.tab(
          <.div(c"card-body", S.spaceySubcontainer)(
            DebateCreationPanel(
              lobby = lobby,
              qualityService = qualityService,
              joinDebate = joinRoomOpt,
              initDebate = sendToMainChannel
            )
          )
        )
    )
  }
}
