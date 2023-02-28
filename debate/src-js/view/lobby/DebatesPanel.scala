package debate
package view.lobby

import cats.implicits._

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
    headings: List[RoomHeading],
    rooms: Set[RoomMetadata],
    storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]],
    presentDebaters: Set[String],
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => Callback
  ) =
    <.div(c"card-body", S.spaceySubcontainer)(
      Local[Boolean].make(true) { anonymizeAll =>
        Local[String].syncedWithSessionStorage("room-name-search", "") { roomNameLive =>
          val canEnter =
            roomNameLive.value.nonEmpty && userName.nonEmpty &&
              rooms.exists(_.name == roomNameLive.value)
          val enter =
            if (canEnter)
              connect(ConnectionSpec(isOfficial, roomNameLive.value, userName))
            else
              Callback.empty

          def statsForStory(room: RoomMetadata) =
            storyRecord.get(userName).flatMap(_.get(room.sourceMaterialId)).combineAll
          val metadatasByHeading = rooms
            .groupBy(room => RoomHeading.infer(room, userName, statsForStory(room)))

          def showMetadatasWithHeading(heading: RoomHeading) = {
            val headingStyle = {
              import RoomHeading._
              heading match {
                case AwaitingFeedback =>
                  S.awaitingFeedbackStatusLabel
                case InProgress =>
                  S.inProgressStatusLabel
                case AssignedForOfflineJudging =>
                  S.assignedForOfflineJudgingStatusLabel
                case CurrentlyOfflineJudging =>
                  S.currentlyOfflineJudgingStatusLabel
                case EligibleForOfflineJudging =>
                  S.eligibleForOfflineJudgingStatusLabel
                case WaitingToBegin =>
                  S.waitingToBeginStatusLabel
                case MustJudgeBeforeDebating =>
                  S.mustJudgeBeforeDebatingStatusLabel
                case Complete =>
                  S.completeStatusLabel
              }
            }
            val roomsForHeading = metadatasByHeading.get(heading).combineAll

            val hideResultsByDefault = Set[RoomHeading](
              RoomHeading.CurrentlyOfflineJudging,
              RoomHeading.EligibleForOfflineJudging,
              RoomHeading.AssignedForOfflineJudging
            ).contains(heading)

            if (roomsForHeading.isEmpty)
              None
            else
              Some {
                ReactFragment(
                  <.h5(headingStyle)(heading.titleString),
                  <.div(S.metadataListContainer, S.spaceySubcontainer) {
                    def showRooms(rooms: Set[RoomMetadata], matches: Boolean) = rooms
                      .toVector
                      .sorted(RoomMetadata.getOrdering(userName, presentDebaters))
                      .toVdomArray { case rm: RoomMetadata =>
                        Local[Boolean].make(hideResultsByDefault) { hideResults =>
                          Local[Boolean].make(true) { anonymize =>
                            MetadataBox(
                              storyRecord = storyRecord,
                              presentDebaters = presentDebaters,
                              roomMetadata = rm,
                              isOfficial = isOfficial,
                              userName = userName,
                              isAdmin = isAdmin,
                              hideResults = hideResults,
                              anonymize = anonymize,
                              sendToMainChannel = sendToMainChannel,
                              enterRoom = connect
                            )(^.key := rm.name, (^.opacity := "0.25").when(!matches))
                          }
                        }
                      }

                    val (matchingRooms, nonMatchingRooms) =
                      if (roomNameLive.value.isEmpty)
                        roomsForHeading -> Set[RoomMetadata]()
                      else
                        roomsForHeading
                          .partition(_.matchesQuery(roomNameLive.value, anonymizeAll.value))

                    ReactFragment(
                      showRooms(matchingRooms, true),
                      showRooms(nonMatchingRooms, false)
                    )
                  }
                )
              }
          }

          ReactFragment(
            Utils.textInputWithEnterButton(
              field = roomNameLive,
              placeholderOpt = Some("Room"),
              buttonContent = "Join",
              isEnabled = canEnter,
              enter = enter
            )(^.marginBottom := 1.rem),
            ReactFragment(
              Utils.tagDelimitedElements(
                headings.flatMap(showMetadatasWithHeading),
                getElement = identity[VdomElement],
                delimiter = <.div(<.hr)
              ): _*
            )
          )
        }
      }
    )

  def apply(
    isAdmin: Boolean,
    lobby: Lobby,
    userName: String,
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => CallbackTo[Unit]
  ) = {

    val (myDebates, notMyDebates) = lobby
      .officialRooms
      .partition(_.roleAssignments.values.toSet.contains(userName))

    import RoomHeading._

    // of the debates I wasn't assigned to participate in live,
    val debatesForOfflineJudging = notMyDebates.filter(room =>
      // either I've judged/am judging it offline,
      RoomStatus
        .complete
        .getOption(room.status)
        .exists(_.offlineJudgingResults.contains(userName)) ||
        // or I _can_ judge it offline.
        Set[RoomHeading](AwaitingFeedback, EligibleForOfflineJudging, CurrentlyOfflineJudging)
          .contains(
            RoomHeading.infer(
              room,
              userName,
              lobby.storyRecord.get(userName).flatMap(_.get(room.sourceMaterialId)).combineAll
            )
          )
    )

    val liveDebateHeadings = List(
      AwaitingFeedback,
      InProgress,
      WaitingToBegin,
      MustJudgeBeforeDebating,
      Complete
    )
    val offlineJudgingHeadings = List(
      AwaitingFeedback,
      CurrentlyOfflineJudging,
      EligibleForOfflineJudging,
      Complete
    )
    val allHeadings = List(
      AwaitingFeedback,
      InProgress,
      WaitingToBegin,
      MustJudgeBeforeDebating,
      CurrentlyOfflineJudging,
      EligibleForOfflineJudging,
      Complete
    )

    def makeTab(isOfficial: Boolean, headings: List[RoomHeading], rooms: Set[RoomMetadata]) = TabNav
      .tab(
        debatesTab(
          isAdmin = isAdmin,
          userName = userName,
          isOfficial = isOfficial,
          headings = headings,
          rooms = rooms,
          storyRecord = lobby.storyRecord,
          presentDebaters = lobby.presentDebaters,
          connect = connect,
          sendToMainChannel = sendToMainChannel
        )
      )

    TabNav("debate-tab", initialTabIndex = 0)(
      "My Live Debates" ->
        makeTab(isOfficial = true, headings = liveDebateHeadings, rooms = myDebates),
      "My Offline Judging" ->
        makeTab(
          isOfficial = true,
          headings = offlineJudgingHeadings,
          rooms = debatesForOfflineJudging
        ),
      "All Official Debates" ->
        makeTab(isOfficial = true, headings = allHeadings, rooms = lobby.officialRooms),
      "Practice Debates" ->
        makeTab(isOfficial = false, headings = allHeadings, rooms = lobby.practiceRooms)
    )
  }
}
