package debate
package view.lobby

import io.circe.generic.JsonCodec
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.util.LocalState2

object DebatesPanel {

  val DebateTabNav = new TabNav[DebatesTab]

  val S = Styles
  val V = new jjm.ui.View(S)

  val LocalString = new LocalState2[String]

  @JsonCodec
  sealed trait DebatesTab extends Product with Serializable {
    import DebatesTab._
    override def toString =
      this match {
        case MyDebates =>
          "My Official Debates"
        case AllOfficialDebates =>
          "All Official Debates"
        case PracticeDebates =>
          "Practice Debates"
      }
  }
  object DebatesTab {
    case object MyDebates          extends DebatesTab
    case object AllOfficialDebates extends DebatesTab
    case object PracticeDebates    extends DebatesTab

    def all = Vector(MyDebates, AllOfficialDebates, PracticeDebates)
  }

  def apply(
    isAdmin: Boolean,
    lobby: Lobby,
    userName: String,
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit]
  ) = {

    val myDebates = lobby.officialRooms.filter(_.roleAssignments.values.toSet.contains(userName))

    DebateTabNav.make("debate-tab", DebatesTab.all, DebatesTab.MyDebates) { tab =>
      import DebatesTab._
      val isOfficial = tab.value != PracticeDebates
      val currentRooms =
        tab.value match {
          case MyDebates =>
            myDebates
          case AllOfficialDebates =>
            lobby.officialRooms
          case PracticeDebates =>
            lobby.practiceRooms
        }

      <.div(c"card-body", S.spaceySubcontainer)(
        LocalString.syncedWithSessionStorage("room-name-search", "") { roomNameLive =>
          val canEnter =
            roomNameLive.value.nonEmpty && userName.nonEmpty &&
              currentRooms.exists(_.name == roomNameLive.value)
          val enter =
            if (canEnter)
              connect(ConnectionSpec(isOfficial, roomNameLive.value, userName))
            else
              Callback.empty

          val (matchingRooms, nonMatchingRooms) =
            if (roomNameLive.value.isEmpty)
              currentRooms -> Set[RoomMetadata]()
            else
              currentRooms.partition(_.matchesQuery(roomNameLive.value))

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
            val hasRooms = currentRooms.exists(_.status == status)
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
    }
  }
}
