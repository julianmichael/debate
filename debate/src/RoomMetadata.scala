package debate

import cats.implicits._
import cats.kernel.Order

import io.circe.generic.JsonCodec
import monocle.macros.Lenses
import monocle.macros.GenPrism

@JsonCodec
sealed trait SourceMaterialId {
  def title: String
}
object SourceMaterialId {
  case class QuALITYStory(id: String, val title: String) extends SourceMaterialId
  case class Custom(val title: String)                   extends SourceMaterialId

  def fromSourceMaterial(sourceMaterial: SourceMaterial) =
    sourceMaterial match {
      case CustomSourceMaterial(title, _) =>
        SourceMaterialId.Custom(title)
      case QuALITYSourceMaterial(articleId, title, _) =>
        SourceMaterialId.QuALITYStory(articleId, title)
    }
}

sealed trait DebateProgressLabel
object DebateProgressLabel {
  case object Assigned         extends DebateProgressLabel
  case object Begun            extends DebateProgressLabel
  case object AwaitingFeedback extends DebateProgressLabel
  case object Complete         extends DebateProgressLabel

  def all = List(Assigned, Begun, AwaitingFeedback, Complete)
}

@JsonCodec
sealed trait RoomStatus {}
object RoomStatus {
  case object WaitingToBegin extends RoomStatus
  case object InProgress     extends RoomStatus
  case class Complete(
    result: DebateResult,
    offlineJudgingResults: Map[String, OfflineJudgingResult],
    feedbackProviders: Set[String]
  ) extends RoomStatus

  val complete = GenPrism[RoomStatus, Complete]
}

@Lenses
@JsonCodec
case class RoomMetadata(
  name: String,
  sourceMaterialId: SourceMaterialId,
  storyTitle: String,
  roleAssignments: Map[LiveDebateRole, String],
  creationTime: Long,
  status: RoomStatus,
  latestUpdateTime: Long,
  peopleWhoHaveSpoken: Set[String], // people who have taken turns
  currentSpeakers: Set[LiveDebateRole],
  currentParticipants: Set[String]
) {
  def result = RoomStatus.complete.getOption(status).map(_.result)

  def matchesQuery(query: String, anonymize: Boolean) = {
    val itemTerms =
      Set(name, storyTitle) ++
        (if (anonymize)
           Set[String]()
         else
           roleAssignments.values.toSet ++ currentParticipants)

    itemMatchesKeywordQuery(itemTerms = itemTerms, queryKeywords = query.split("\\s+").toSet)
  }

  def getProgressLabel(person: String) =
    status match {
      case RoomStatus.WaitingToBegin =>
        DebateProgressLabel.Assigned
      case RoomStatus.InProgress =>
        if (peopleWhoHaveSpoken.contains(person)) {
          DebateProgressLabel.Begun
        } else
          DebateProgressLabel.Assigned
      case RoomStatus.Complete(_, _, feedbackProviders) =>
        if (feedbackProviders.contains(person))
          DebateProgressLabel.Complete
        else {
          // XXX: just until we import the old feedback results, only ask for feedback for debates created as of 2023
          if (creationTime < timeBeforeWhichToIgnoreMissingFeedback) {
            DebateProgressLabel.Complete
          } else
            DebateProgressLabel.AwaitingFeedback
        }
    }

}
object RoomMetadata {
  def getOrder(userName: String, presentDebaters: Set[String]): Order[RoomMetadata] = Order.by {
    room =>
      val assignedLiveParticipants  = room.roleAssignments.values.toSet
      val numParticipantsNotInLobby = (assignedLiveParticipants -- presentDebaters).size
      val numParticipantsNotInRoom  = (assignedLiveParticipants -- room.currentParticipants).size
      val myRoles                   = room.roleAssignments.filter(_._2 == userName).keySet
      val isMyTurn                  = myRoles.intersect(room.currentSpeakers).nonEmpty
      (!isMyTurn, numParticipantsNotInLobby, -numParticipantsNotInRoom, -room.latestUpdateTime)
  }
  def getOrdering(userName: String, presentDebaters: Set[String]) = catsKernelOrderingForOrder(
    getOrder(userName, presentDebaters)
  )

  def constructStoryRecord(
    rooms: Set[RoomMetadata]
  ): Map[String, Map[SourceMaterialId, DebaterStoryStats]] = rooms.unorderedFoldMap { room =>
    val live =
      room
        .roleAssignments
        .view
        .map { case (role, debater) =>
          val label =
            room.status match {
              case RoomStatus.WaitingToBegin =>
                DebateProgressLabel.Assigned
              case RoomStatus.InProgress =>
                if (room.peopleWhoHaveSpoken.contains(debater)) {
                  DebateProgressLabel.Begun
                } else
                  DebateProgressLabel.Assigned
              case RoomStatus.Complete(_, _, feedbackProviders) =>
                if (feedbackProviders.contains(debater))
                  DebateProgressLabel.Complete
                else {
                  // XXX: just until we import the old feedback results, only ask for feedback for debates created as of 2023
                  if (room.creationTime < timeBeforeWhichToIgnoreMissingFeedback) {
                    DebateProgressLabel.Complete
                  } else
                    DebateProgressLabel.AwaitingFeedback

                  // DebateProgressLabel.AwaitingFeedback
                }
            }
          val stats =
            if (role == Judge) {
              DebaterStoryStats(liveJudging = Map(label -> Set(room.name)))
            } else
              DebaterStoryStats(debating = Map(label -> Set(room.name)))

          Map(debater -> Map(room.sourceMaterialId -> stats))
        }
        .toVector
        .combineAll

    // TODO: add assignments for offline judging
    val offline = RoomStatus
      .complete
      .getOption(room.status)
      .foldMap { case RoomStatus.Complete(_, offlineJudgingResults, feedbackProviders) =>
        offlineJudgingResults
          .keySet
          .view
          .map { judge =>
            val label =
              if (feedbackProviders.contains(judge)) {
                DebateProgressLabel.Complete
              } else {

                // XXX: just until we import the old feedback results, only ask for feedback for debates created as of 2023
                if (room.creationTime < timeBeforeWhichToIgnoreMissingFeedback) {
                  DebateProgressLabel.Complete
                } else
                  DebateProgressLabel.AwaitingFeedback

                // DebateProgressLabel.AwaitingFeedback
              }
            val stats = DebaterStoryStats(offlineJudging = Map(label -> Set(room.name)))
            judge -> Map(room.sourceMaterialId -> stats)
          }
          .toMap
      }

    live |+| offline
  }
}
