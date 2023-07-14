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
    profiles: Map[String, Profile],
    isAdmin: Boolean,
    userName: String,
    isOfficial: Boolean,
    headings: List[RoomHeading],
    rooms: Set[RoomMetadata],
    storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]],
    presentDebaters: Set[String],
    connect: ConnectionSpec => Callback,
    sendToMainChannel: MainChannelRequest => Callback,
    includeAnalytics: Boolean
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
                case MustJudgeBeforeSeeing =>
                  S.mustJudgeBeforeSeeingStatusLabel
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
                      .sorted(
                        RoomMetadata.getOrdering(
                          userName,
                          presentDebaters,
                          storyRecord,
                          heading.showCompleteDebatesLast
                        )
                      )
                      .toVdomArray { case rm: RoomMetadata =>
                        Local[Boolean].make(hideResultsByDefault) { hideResults =>
                          Local[Boolean].make(true) { anonymize =>
                            MetadataBox(
                              storyRecord = storyRecord,
                              profiles = profiles,
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
            Option(PersonalizedGraphsCard(userName)).filter(_ => includeAnalytics),
            Utils.textInputWithEnterButton(
              field = roomNameLive,
              placeholderOpt = Some("Room"),
              buttonContent = "Join",
              isEnabled = canEnter,
              enter = enter
            )(^.marginBottom := 1.rem, ^.marginTop := 1.rem),
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
        Set[RoomHeading](
          AwaitingFeedback,
          CurrentlyOfflineJudging,
          AssignedForOfflineJudging,
          EligibleForOfflineJudging
        ).contains(
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
      MustJudgeBeforeSeeing,
      Complete
    )
    val offlineJudgingHeadings = List(
      AwaitingFeedback,
      CurrentlyOfflineJudging,
      AssignedForOfflineJudging,
      EligibleForOfflineJudging,
      Complete
    )
    val allHeadings = List(
      AwaitingFeedback,
      InProgress,
      WaitingToBegin,
      MustJudgeBeforeSeeing,
      CurrentlyOfflineJudging,
      AssignedForOfflineJudging,
      EligibleForOfflineJudging,
      Complete
    )

    def makeTab(
      isOfficial: Boolean,
      headings: List[RoomHeading],
      rooms: Set[RoomMetadata],
      includeAnalytics: Boolean
    ) = debatesTab(
      profiles = lobby.profiles,
      isAdmin = isAdmin,
      userName = userName,
      isOfficial = isOfficial,
      headings = headings,
      rooms = rooms,
      storyRecord = lobby.storyRecord,
      presentDebaters = lobby.presentDebaters,
      connect = connect,
      sendToMainChannel = sendToMainChannel,
      includeAnalytics = includeAnalytics
    )

    // TODO: eliminate redundancy between this and notification counts in LobbyPage

    val numDebatesMyTurn =
      lobby
        .officialRooms
        .filter(_.roleAssignments.values.toSet.contains(userName))
        .filter { room =>
          val myRoles = room.roleAssignments.filter(_._2 == userName).keySet
          myRoles.intersect(room.currentSpeakers).nonEmpty
        }
        .size

    val numDebatesReadyToJudge =
      lobby
        .officialRooms
        .filter(_.offlineJudgeAssignments.contains(userName))
        .flatMap(r => RoomStatus.complete.getOption(r.status))
        .filter(_.offlineJudgingResults.get(userName).forall(_.result.isEmpty))
        .size

    TabNav("debate-tab", initialTabIndex = 0)(
      "My Live Debates" ->
        TabNav.tabWithNotifications(numDebatesMyTurn)(
          makeTab(
            isOfficial = true,
            headings = liveDebateHeadings,
            rooms = myDebates,
            includeAnalytics = true
          )
        ),
      "My Offline Judging" ->
        TabNav.tabWithNotifications(numDebatesReadyToJudge)(
          makeTab(
            isOfficial = true,
            headings = offlineJudgingHeadings,
            rooms = debatesForOfflineJudging,
            includeAnalytics = false
          )
        ),
      "All Official Debates" ->
        TabNav.tab(
          makeTab(
            isOfficial = true,
            headings = allHeadings,
            rooms = lobby.officialRooms,
            includeAnalytics = false
          )
        ),
      "Practice Debates" ->
        TabNav.tab(
          makeTab(
            isOfficial = false,
            headings = allHeadings,
            rooms = lobby.practiceRooms,
            includeAnalytics = false
          )
        )
    )
  }
}
