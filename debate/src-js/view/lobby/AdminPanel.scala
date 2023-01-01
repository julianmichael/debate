package debate
package view.lobby

import cats.Order
import cats.implicits._

import io.circe.generic.JsonCodec
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

  @JsonCodec
  sealed trait AdminTab extends Product with Serializable {
    import AdminTab._
    override def toString =
      this match {
        case Profiles =>
          "Profiles"
        case CreateDebate =>
          "Create Debate"
      }
  }
  object AdminTab {
    case object Profiles     extends AdminTab
    case object CreateDebate extends AdminTab

    def all: Vector[AdminTab] = Vector(Profiles, CreateDebate)
  }

  val AdminTabNav = new TabNav[AdminTab]
  val LocalString = new LocalState2[String]

  def debaterCard(
    lobby: Lobby,
    name: String,
    joinOfficialRoom: String => Callback,
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
                room => <.a(^.href := "#", room.name, ^.onClick --> joinOfficialRoom(room.name))
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

  def apply(
    lobby: Lobby,
    qualityService: QuALITYService[AsyncCallback],
    userName: String,
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => Callback
  ) = {
    val joinOfficialRoom = (roomName: String) => connect(ConnectionSpec(true, roomName, userName))
    AdminTabNav.make("admin-tab", AdminTab.all, AdminTab.CreateDebate) { tab =>
      import AdminTab._
      <.div(c"card-body", S.spaceySubcontainer)(
        tab.value match {
          case Profiles =>
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
                    newProfileStr.value.nonEmpty &&
                      !lobby.trackedDebaters.contains(newProfileStr.value),
                  enter =
                    sendToMainChannel(RegisterDebater(newProfileStr.value)) >>
                      newProfileStr.setState("")
                )(^.marginBottom := "1rem"),
                <.h3("Active Profiles"),
                <.div(S.profileListContainer, S.spaceySubcontainer)(
                  lobby
                    .trackedDebaters
                    .toVector
                    .sorted(catsKernelOrderingForOrder(profileOrder))
                    .toVdomArray { name =>
                      debaterCard(lobby, name, joinOfficialRoom, sendToMainChannel)(
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
                      debaterCard(lobby, name, joinOfficialRoom, sendToMainChannel)(
                        ^.key := name,
                        S.simpleUnselectable.when(!profileMatchesQuery(name))
                      )
                    }
                )
              )
            }
          case CreateDebate =>
            DebateCreationPanel(
              lobby = lobby,
              qualityService = qualityService,
              joinDebate = Option(userName)
                .filter(_.nonEmpty)
                .map(userName =>
                  (isOfficial: Boolean, roomName: String) =>
                    connect(ConnectionSpec(isOfficial, roomName, userName))
                ),
              initDebate = sendToMainChannel
            )
        }
      )
    }
  }

}
