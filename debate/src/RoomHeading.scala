package debate

import debate.adminUsername

trait RoomHeading {
  import RoomHeading._

  def showCompleteDebatesLast: Boolean =
    this match {
      case AssignedForOfflineJudging | CurrentlyOfflineJudging | EligibleForOfflineJudging =>
        false
      case _ =>
        true
    }

  override def toString =
    this match {
      case AwaitingFeedback =>
        "awaiting feedback"
      case InProgress =>
        "in progress"
      case WaitingToBegin =>
        "waiting to begin"
      case MustJudgeBeforeSeeing =>
        "must judge before seeing"
      case AssignedForOfflineJudging =>
        "you are assigned as an offline judge"
      case CurrentlyOfflineJudging =>
        "you are judging offline"
      case EligibleForOfflineJudging =>
        "eligible for offline judging"
      case Complete =>
        "complete"
    }

  def titleString =
    this match {
      case AwaitingFeedback =>
        "Awaiting Your Feedback"
      case InProgress =>
        "In Progress"
      case WaitingToBegin =>
        "Waiting to Begin"
      case MustJudgeBeforeSeeing =>
        "Must Judge the Story Before Seeing"
      case AssignedForOfflineJudging =>
        "You are Assigned to Judge Offline"
      case CurrentlyOfflineJudging =>
        "You are Judging Offline"
      case EligibleForOfflineJudging =>
        "Eligible for You to Judge"
      case Complete =>
        "Complete"
    }
}
object RoomHeading {
  case object AwaitingFeedback          extends RoomHeading
  case object InProgress                extends RoomHeading
  case object WaitingToBegin            extends RoomHeading
  case object CurrentlyOfflineJudging   extends RoomHeading
  case object AssignedForOfflineJudging extends RoomHeading
  case object EligibleForOfflineJudging extends RoomHeading
  case object MustJudgeBeforeSeeing     extends RoomHeading
  case object Complete                  extends RoomHeading

  def infer(metadata: RoomMetadata, user: String, stats: DebaterStoryStats): RoomHeading =
    metadata.status match {
      case RoomStatus.Complete(_, offlineJudging, feedbackProviders) =>
        if (metadata.roleAssignments.values.toSet.contains(user)) {
          if (feedbackProviders.contains(user)) {
            Complete
          } else {
            // NOTE: only ask for feedback for debates created as of 2023.
            // Not gonna bother importing the old feedback results.
            if (metadata.creationTime < timeBeforeWhichToIgnoreMissingFeedback) {
              Complete
            } else
              AwaitingFeedback
          }
        } else if (offlineJudging.get(user).exists(_.result.isEmpty)) {
          CurrentlyOfflineJudging
        } else if (
          metadata.offlineJudgeAssignments.contains(user) &&
          !offlineJudging.get(user).exists(_.result.nonEmpty)
        ) {
          AssignedForOfflineJudging
        } else if (offlineJudging.get(user).exists(_.result.nonEmpty)) {
          if (feedbackProviders.contains(user)) {
            Complete
          } else {
            // NOTE: only ask for feedback for debates created as of 2023.
            // Not gonna bother importing the old feedback results.
            if (metadata.creationTime < timeBeforeWhichToIgnoreMissingFeedback) {
              Complete
            } else
              AwaitingFeedback
          }
        } else if (stats.needsToJudgeStory) {
          MustJudgeBeforeSeeing
        } else if (stats.hasReadStory || !stats.canJudgeMore || user == adminUsername) {
          Complete
        } else
          EligibleForOfflineJudging
      case _ if metadata.offlineJudgeAssignments.contains(user) =>
        AssignedForOfflineJudging
      case _ if !metadata.roleAssignments.values.toSet.contains(user) && stats.needsToJudgeStory =>
        MustJudgeBeforeSeeing
      case _
          if !metadata.roleAssignments.values.toSet.contains(user) && !stats.hasReadStory &&
            stats.canJudgeMore && user != adminUsername =>
        EligibleForOfflineJudging
      case RoomStatus.InProgress =>
        InProgress
      case RoomStatus.WaitingToBegin =>
        WaitingToBegin
    }
}
