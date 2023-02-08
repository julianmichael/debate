package debate

trait RoomHeading {
  import RoomHeading._

  override def toString =
    this match {
      case AwaitingFeedback =>
        "awaiting feedback"
      case InProgress =>
        "in progress"
      case WaitingToBegin =>
        "waiting to begin"
      case MustJudgeBeforeDebating =>
        "must judge before debating"
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
      case MustJudgeBeforeDebating =>
        "Must Judge the Story Before Debating"
      case EligibleForOfflineJudging =>
        "Eligible for You to Judge"
      case Complete =>
        "Complete"
    }
}
object RoomHeading {
  case object AwaitingFeedback          extends RoomHeading
  case object EligibleForOfflineJudging extends RoomHeading
  case object InProgress                extends RoomHeading
  case object WaitingToBegin            extends RoomHeading
  case object MustJudgeBeforeDebating   extends RoomHeading
  case object Complete                  extends RoomHeading

  def infer(metadata: RoomMetadata, user: String, stats: DebaterStoryStats): RoomHeading =
    metadata.status match {
      case RoomStatus.Complete(_, offlineJudging, feedbackProviders) =>
        if (metadata.roleAssignments.values.toSet.contains(user)) {
          if (feedbackProviders.contains(user)) {
            Complete
          } else {
            AwaitingFeedback
          }
        } else if (offlineJudging.contains(user) || stats.hasReadStory) {
          Complete
        } else
          EligibleForOfflineJudging
      case _
          if metadata
            .roleAssignments
            .toVector
            .existsAs { case (Debater(_), `user`) =>
              true
            } && stats.needsToJudgeStory =>
        MustJudgeBeforeDebating
      case RoomStatus.InProgress =>
        InProgress
      case RoomStatus.WaitingToBegin =>
        WaitingToBegin
    }
}
