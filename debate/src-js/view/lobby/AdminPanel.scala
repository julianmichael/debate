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
import debate.util.Local
import debate.DebateProgressLabel.Assigned
import debate.DebateProgressLabel.Begun
import debate.DebateProgressLabel.AwaitingFeedback
import debate.DebateProgressLabel.Complete

object AdminPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  def debateProgressLabelStyle(label: DebateProgressLabel) =
    label match {
      case Assigned =>
        S.waitingToBeginStatusLabel
      case Begun =>
        S.inProgressStatusLabel
      case AwaitingFeedback =>
        S.awaitingFeedbackStatusLabel
      case Complete =>
        S.completeStatusLabel
    }

  def debaterCard(
    lobby: Lobby,
    name: String,
    joinOfficialRoomOpt: Option[String => Callback],
    sendToMainChannel: MainChannelRequest => Callback
  ) = {
    val debatesByName = lobby.officialRooms.view.map(r => r.name -> r).toMap

    def showStories(title: String, stories: Map[SourceMaterialId, DebaterStoryStats]) =
      if (stories.isEmpty)
        None
      else
        Some {
          ReactFragment(
            <.h6(c"mt-2")(title, s" (${stories.size})"),
            <.div(S.storyListContainer)(
              stories.toVdomArray { case (sourceMaterialId, stats) =>
                def showStats(stats: Map[DebateProgressLabel, Set[String]]) = {
                  val debatesInRevChronOrder = stats
                    .view
                    .flatMap { case (label, names) =>
                      names.view.map(name => debatesByName(name) -> label).toVector
                    }
                    .toVector
                    .sortBy(-_._1.latestUpdateTime)
                  Utils
                    .tagDelimitedTags[Vector, (RoomMetadata, DebateProgressLabel)](
                      debatesInRevChronOrder,
                      delimiter = <.br(),
                      getTag = { case (room, label) =>
                        <.a(debateProgressLabelStyle(label))(
                          ^.href := "#",
                          room.name,
                          joinOfficialRoomOpt.whenDefined(join => ^.onClick --> join(room.name))
                        )
                      }
                    )
                    .toVdomArray
                }

                <.div(c"card")(
                  <.div(c"card-body p-2")(
                    <.div(c"card-text small")(
                      <.div(S.bold)(sourceMaterialId.title),
                      <.div("Debating", <.br(), showStats(stats.debating))
                        .when(stats.debating.nonEmpty),
                      <.div("Live Judging", <.br(), showStats(stats.liveJudging))
                        .when(stats.liveJudging.nonEmpty),
                      <.div("Offline Judging", <.br(), showStats(stats.offlineJudging))
                        .when(stats.offlineJudging.nonEmpty)
                    )
                  )
                )
              }
            )
          )
        }

    val stories                       = lobby.storyRecord.get(name).combineAll
    val (storiesRead, storiesNotRead) = stories.partition(x => (x._2.debating - Assigned).nonEmpty)
    val (storiesAssignedToRead, storiesNotAssignedToRead) = storiesNotRead
      .partition(x => !storiesRead.contains(x._1) && x._2.debating.contains(Assigned))
    val (storiesJudged, storiesNotJudged) = storiesNotAssignedToRead
      .partition(x => (x._2.allJudging - Assigned).nonEmpty)
    val storiesAssignedToJudge = storiesNotJudged.filter(_._2.allJudging.nonEmpty)

    <.div(c"card")(
      <.div(c"card-body")(
        <.h4(c"card-title")(name),
        <.h5(s"Stories"),
        showStories("Read", storiesRead),
        showStories("Assigned to Read", storiesAssignedToRead),
        showStories("Judged", storiesJudged),
        showStories("Assigned to Judge", storiesAssignedToJudge),
        if (lobby.trackedDebaters.contains(name)) {
          <.button(c"mt-2 btn btn-sm btn-outline-danger", S.simpleSelectable)(
            <.i(c"bi bi-x"),
            " Deactivate",
            ^.onClick --> sendToMainChannel(RemoveDebater(name))
          )
        } else {
          <.div(^.key := name)(
            <.button(c"mt-2 btn btn-sm btn-outline-secondary", S.simpleSelectable)(
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
    Local[String].make("") { newProfileStr =>
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
