package debate
package view.lobby

import cats.Order
import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.quality.QuALITYService
import debate.util.LocalState2

object AdminPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  val LocalString = new LocalState2[String]

  def debaterCard(
    lobby: Lobby,
    name: String,
    joinOfficialRoomOpt: Option[String => Callback],
    sendToMainChannel: MainChannelRequest => Callback
  ) = {
    val debates = lobby.officialRooms.filter(_.roleAssignments.values.exists(_ == name))
    <.div(c"card")(
      <.div(c"card-body")(
        <.h6(c"card-title")(name),
        <.p(c"card-text small")(
          s"Debates: ${debates.size}",
          <.br(),
          Utils
            .delimitedTags[Vector, RoomMetadata](
              debates.toVector.sortBy(-_.latestUpdateTime),
              getTag =
                room =>
                  <.a(
                    ^.href := "#",
                    room.name,
                    joinOfficialRoomOpt.whenDefined(join => ^.onClick --> join(room.name))
                  )
            )
            .toVdomArray
        ),
        if (lobby.trackedDebaters.contains(name)) {
          <.button(c"btn btn-sm btn-outline-danger", S.simpleSelectable)(
            <.i(c"bi bi-x"),
            " Deactivate",
            ^.onClick --> sendToMainChannel(RemoveDebater(name))
          )
        } else {
          <.div(^.key := name)(
            <.button(c"btn btn-sm btn-outline-secondary", S.simpleSelectable)(
              <.i(c"bi bi-arrow-up"),
              " Reactivate",
              ^.onClick --> sendToMainChannel(RegisterDebater(name))
            )
          )
        }
      )
    )
  }

  def profilesTab(
    lobby: Lobby,
    joinOfficialRoomOpt: Option[String => Callback],
    sendToMainChannel: MainChannelRequest => Callback
  ) =
    LocalString.make("") { newProfileStr =>
      def profileMatchesQuery(profile: String) = itemMatchesKeywordQuery(
        itemTerms = Set(profile),
        queryKeywords = newProfileStr.value.split("\\s+").toSet
      )

      val profileOrder = {
        import Order._
        whenEqual(reverse(by[String, Boolean](profileMatchesQuery)), Order[String])
      }

      ReactFragment(
        Utils.textInputWithEnterButton(
          field = newProfileStr,
          placeholderOpt = None,
          buttonContent = <.i(c"bi bi-plus"),
          isEnabled =
            newProfileStr.value.nonEmpty && !lobby.trackedDebaters.contains(newProfileStr.value),
          enter =
            sendToMainChannel(RegisterDebater(newProfileStr.value)) >> newProfileStr.setState("")
        )(^.marginBottom := "1rem"),
        <.h3("Active Profiles"),
        <.div(S.profileListContainer, S.spaceySubcontainer)(
          lobby
            .trackedDebaters
            .toVector
            .sorted(catsKernelOrderingForOrder(profileOrder))
            .toVdomArray { name =>
              debaterCard(lobby, name, joinOfficialRoomOpt, sendToMainChannel)(
                ^.key := name,
                S.simpleUnselectable.when(!profileMatchesQuery(name))
              )

            }
        ),
        <.div(<.hr()),
        <.h3("Inactive Profiles"),
        <.div(S.profileListContainer, S.spaceySubcontainer)(
          (lobby.allDebaters -- lobby.trackedDebaters)
            .toVector
            .sorted(catsKernelOrderingForOrder(profileOrder))
            .toVdomArray { name =>
              debaterCard(lobby, name, joinOfficialRoomOpt, sendToMainChannel)(
                ^.key := name,
                S.simpleUnselectable.when(!profileMatchesQuery(name))
              )
            }
        )
      )
    }

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
            profilesTab(
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
