package debate
package view.lobby

import cats.implicits._

import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

import debate.Utils.ClassSetInterpolator
import debate.util.Local

trait RoomHeading {
  import RoomHeading._

  override def toString =
    this match {
      case AwaitingFeedback =>
        "awaiting feedback"
      case EligibleForOfflineJudging =>
        "eligible for offline judging"
      case InProgress =>
        "in progress"
      case WaitingToBegin =>
        "waiting to begin"
      case Complete =>
        "complete"
    }

  // def isComplete =
  //   this match {
  //     case Complete =>
  //       true
  //     case _ =>
  //       false
  //   }

  def titleString =
    this match {
      case AwaitingFeedback =>
        "Awaiting Your Feedback"
      case EligibleForOfflineJudging =>
        "Eligible for You to Judge"
      case InProgress =>
        "In Progress"
      case WaitingToBegin =>
        "Waiting to Begin"
      case Complete =>
        "Complete"
    }
}
object RoomHeading {
  case object AwaitingFeedback          extends RoomHeading
  case object EligibleForOfflineJudging extends RoomHeading
  case object InProgress                extends RoomHeading
  case object WaitingToBegin            extends RoomHeading
  case object Complete                  extends RoomHeading

  def infer(metadata: RoomMetadata, user: String): RoomHeading =
    metadata.status match {
      case RoomStatus.InProgress =>
        InProgress
      case RoomStatus.WaitingToBegin =>
        WaitingToBegin
      case RoomStatus.Complete(_, offlineJudging, feedbackProviders) =>
        if (metadata.roleAssignments.values.toSet.contains(user)) {
          if (feedbackProviders.contains(user)) {
            Complete
          } else {
            AwaitingFeedback
          }
        } else if (offlineJudging.contains(user)) {
          Complete
        } else
          EligibleForOfflineJudging
    }
}

object DebatesPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  def debatesTab(
    isAdmin: Boolean,
    userName: String,
    isOfficial: Boolean,
    headings: List[RoomHeading],
    rooms: Set[RoomMetadata],
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

          val metadatasByHeading = rooms.groupBy(RoomHeading.infer(_, userName))

          def showMetadatasWithHeading(heading: RoomHeading) = {
            val headingStyle = {
              import RoomHeading._
              heading match {
                case AwaitingFeedback =>
                  S.awaitingFeedbackStatusLabel
                case InProgress =>
                  S.inProgressStatusLabel
                case EligibleForOfflineJudging =>
                  S.eligibleForOfflineJudgingStatusLabel
                case WaitingToBegin =>
                  S.waitingToBeginStatusLabel
                case Complete =>
                  S.completeStatusLabel
              }
            }
            val roomsForHeading = metadatasByHeading.get(heading).combineAll
            ReactFragment(
              <.h5(headingStyle)(heading.titleString),
              <.div(S.metadataListContainer, S.spaceySubcontainer)(
                if (roomsForHeading.isEmpty) {
                  <.div("No rooms to show.")
                } else {
                  def showRooms(rooms: Set[RoomMetadata], matches: Boolean) = rooms
                    .toVector
                    .sorted(RoomMetadata.getOrdering(userName))
                    .toVdomArray { case rm: RoomMetadata =>
                      Local[Boolean]
                        .make(heading == RoomHeading.EligibleForOfflineJudging) { hideResults =>
                          Local[Boolean].make(true) { anonymize =>
                            MetadataBox(
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

                  ReactFragment(showRooms(matchingRooms, true), showRooms(nonMatchingRooms, false))
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
            ReactFragment(
              Utils.tagDelimitedElements(
                headings,
                getElement = showMetadatasWithHeading,
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
    val liveDebateHeadings     = List(AwaitingFeedback, InProgress, WaitingToBegin, Complete)
    val offlineJudgingHeadings = List(AwaitingFeedback, EligibleForOfflineJudging, Complete)
    val allHeadings = List(
      AwaitingFeedback,
      InProgress,
      EligibleForOfflineJudging,
      WaitingToBegin,
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
          connect = connect,
          sendToMainChannel = sendToMainChannel
        )
      )

    TabNav("debate-tab", initialTabIndex = 0)(
      "My Live Debates" ->
        makeTab(isOfficial = true, headings = liveDebateHeadings, rooms = myDebates),
      "My Offline Judging" ->
        makeTab(isOfficial = true, headings = offlineJudgingHeadings, rooms = notMyDebates),
      "All Official Debates" ->
        makeTab(isOfficial = true, headings = allHeadings, rooms = lobby.officialRooms),
      "Practice Debates" ->
        makeTab(isOfficial = false, headings = allHeadings, rooms = lobby.practiceRooms)
    )
  }
}
