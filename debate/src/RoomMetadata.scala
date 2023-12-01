package debate

import cats.implicits._
import cats.kernel.Order

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import monocle.macros.Lenses

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
    offlineJudgingResults: Map[String, OfflineJudgment],
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
  question: String,
  roleAssignments: Map[LiveDebateRole, String],
  offlineJudgeAssignments: Map[String, Option[OfflineJudgingMode]],
  creationTime: Long,
  status: RoomStatus,
  latestUpdateTime: Long,
  peopleWhoHaveSpoken: Set[String], // people who have taken turns
  currentSpeakers: Set[LiveDebateRole],
  currentParticipants: Set[String]
) {
  def result = RoomStatus.complete.getOption(status).map(_.result)

  def participants = roleAssignments.values.toSet ++ offlineJudgeAssignments.keySet

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
          // NOTE: only ask for feedback for debates created as of 2023
          // not gonna bother importing the old feedback results
          if (creationTime < timeBeforeWhichToIgnoreMissingFeedback) {
            DebateProgressLabel.Complete
          } else
            DebateProgressLabel.AwaitingFeedback
        }
    }

}
object RoomMetadata {

  def getOrderKey(
    userName: String,
    presentDebaters: Set[String],
    storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]]
  )(room: RoomMetadata) = {
    // TODO refactor so this isn't duplicated inside MetadataBox
    val stats = storyRecord.get(userName).flatMap(_.get(room.sourceMaterialId)).combineAll
    val debatesUserMustJudgeFirst = stats.debatesUserMustJudgeFirst(room.name)

    val mustWaitForDebateToEnd =
      room.result.isEmpty &&
        (room.offlineJudgeAssignments.contains(userName) ||
          (!stats.hasReadStory && stats.canJudgeMore))

    val canEnterRoom =
      userName.nonEmpty && debatesUserMustJudgeFirst.isEmpty && !mustWaitForDebateToEnd

    val assignedLiveParticipants  = room.roleAssignments.values.toSet
    val numParticipantsNotInLobby = (assignedLiveParticipants -- presentDebaters).size
    val numParticipantsNotInRoom  = (assignedLiveParticipants -- room.currentParticipants).size
    val myRoles                   = room.roleAssignments.filter(_._2 == userName).keySet
    val isMyTurn                  = myRoles.intersect(room.currentSpeakers).nonEmpty

    if (room.result.isEmpty) {
      Left(
        (
          !isMyTurn,
          !canEnterRoom,
          numParticipantsNotInLobby,
          -numParticipantsNotInRoom,
          -room.latestUpdateTime
        )
      )
    } else
      Right(-room.creationTime)
  }

  def getOrder(
    userName: String,
    presentDebaters: Set[String],
    storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]],
    showCompleteDebatesLast: Boolean
  ): Order[RoomMetadata] =
    if (showCompleteDebatesLast)
      Order.by(getOrderKey(userName, presentDebaters, storyRecord))
    else
      Order.by { room =>
        getOrderKey(userName, presentDebaters, storyRecord)(room).swap
      }

  def getOrdering(
    userName: String,
    presentDebaters: Set[String],
    storyRecord: Map[String, Map[SourceMaterialId, DebaterStoryStats]],
    showCompleteDebatesLast: Boolean
  ) = catsKernelOrderingForOrder(
    getOrder(userName, presentDebaters, storyRecord, showCompleteDebatesLast)
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
                  // only ask for feedback for debates created as of 2023
                  // not gonna bother importing the old feedback results
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

    val offline =
      RoomStatus
        .complete
        .getOption(room.status)
        .foldMap { case RoomStatus.Complete(_, offlineJudgingResults, feedbackProviders) =>
          offlineJudgingResults
            .toVector
            .view
            .map { case (judge, judgment) =>
              val label =
                if (feedbackProviders.contains(judge)) {
                  DebateProgressLabel.Complete
                } else if (judgment.result.nonEmpty) {
                  // only ask for feedback for debates created as of 2023
                  // not gonna bother importing the old feedback results
                  if (room.creationTime < timeBeforeWhichToIgnoreMissingFeedback) {
                    DebateProgressLabel.Complete
                  } else
                    DebateProgressLabel.AwaitingFeedback
                  // DebateProgressLabel.AwaitingFeedback
                } else { // currently judging, haven't finished
                  DebateProgressLabel.Begun
                }
              val stats = DebaterStoryStats(offlineJudging = Map(label -> Set(room.name)))
              judge -> Map(room.sourceMaterialId -> stats)
            }
            .toMap
        } |+|
        (
          room.offlineJudgeAssignments --
            RoomStatus.complete.getOption(room.status).foldMap(_.offlineJudgingResults.keySet)
        ).toVector
          .view
          .map { case (judge, _) =>
            judge ->
              Map(
                room.sourceMaterialId ->
                  DebaterStoryStats(offlineJudging =
                    Map(DebateProgressLabel.Assigned -> Set(room.name))
                  )
              )
          }
          .toMap

    live |+| offline
  }
}
