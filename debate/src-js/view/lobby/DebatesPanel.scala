package debate
package view.lobby

import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.util.Local

object DebatesPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  def debatesTab(
    isAdmin: Boolean,
    userName: String,
    isOfficial: Boolean,
    rooms: Set[RoomMetadata],
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => Callback
  ) =
    <.div(c"card-body", S.spaceySubcontainer)(
      Local[String].syncedWithSessionStorage("room-name-search", "") { roomNameLive =>
        val canEnter =
          roomNameLive.value.nonEmpty && userName.nonEmpty &&
            rooms.exists(_.name == roomNameLive.value)
        val enter =
          if (canEnter)
            connect(ConnectionSpec(isOfficial, roomNameLive.value, userName))
          else
            Callback.empty

        val (matchingRooms, nonMatchingRooms) =
          if (roomNameLive.value.isEmpty)
            rooms -> Set[RoomMetadata]()
          else
            rooms.partition(_.matchesQuery(roomNameLive.value))

        def makeMetadatas(status: RoomStatus) = {
          val statusStyle = {
            import RoomStatus._
            status match {
              case WaitingToBegin =>
                S.waitingToBeginStatusLabel
              case InProgress =>
                S.inProgressStatusLabel
              case Complete =>
                S.completeStatusLabel
            }
          }
          val hasRooms = rooms.exists(_.status == status)
          ReactFragment(
            <.h5(statusStyle)(status.titleString),
            <.div(S.metadataListContainer, S.spaceySubcontainer)(
              if (!hasRooms) {
                <.div("No rooms to show.")
              } else {
                def showRooms(rooms: Set[RoomMetadata], matches: Boolean) = rooms
                  .toVector
                  .sorted(RoomMetadata.getOrdering(userName))
                  .toVdomArray { case rm: RoomMetadata =>
                    MetadataBox(
                      isAdmin = isAdmin,
                      roomMetadata = rm,
                      isOfficial = isOfficial,
                      userName = userName,
                      sendToMainChannel = sendToMainChannel,
                      enterRoom = connect
                    )(^.key := rm.name, (^.opacity := "0.25").when(!matches))
                  }

                ReactFragment(
                  showRooms(matchingRooms.filter(_.status == status), true),
                  showRooms(nonMatchingRooms.filter(_.status == status), false)
                )
              }
            )
          )
        }

        ReactFragment(
          Utils.textInputWithEnterButton(
            field = roomNameLive,
            placeholderOpt = Some("Room"),
            buttonContent = "Join",
            isEnabled = canEnter,
            enter = enter
          )(^.marginBottom := 1.rem),
          makeMetadatas(RoomStatus.InProgress),
          <.div(<.hr),
          makeMetadatas(RoomStatus.WaitingToBegin),
          <.div(<.hr),
          makeMetadatas(RoomStatus.Complete)
        )
      }
    )

  def apply(
    isAdmin: Boolean,
    lobby: Lobby,
    userName: String,
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit]
  ) = {

    val myDebates = lobby.officialRooms.filter(_.roleAssignments.values.toSet.contains(userName))

    def makeTab(isOfficial: Boolean, rooms: Set[RoomMetadata]) = TabNav.tab(
      debatesTab(
        isAdmin = isAdmin,
        userName = userName,
        isOfficial = isOfficial,
        rooms = rooms,
        connect = connect,
        sendToMainChannel = sendToMainChannel
      )
    )

    TabNav("debate-tab", initialTabIndex = 0)(
      "My Debates"           -> makeTab(isOfficial = true, rooms = myDebates),
      "All Official Debates" -> makeTab(isOfficial = true, rooms = lobby.officialRooms),
      "Practice Debates"     -> makeTab(isOfficial = false, rooms = lobby.practiceRooms)
    )
  }
}
