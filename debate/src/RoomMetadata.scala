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
  roleAssignments: Map[DebateRole, String],
  creationTime: Long,
  status: RoomStatus,
  latestUpdateTime: Long,
  peopleWhoHaveSpoken: Set[String], // people who have taken turns
  currentSpeakers: Set[DebateRole],
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
        else
          DebateProgressLabel.AwaitingFeedback
    }

}
object RoomMetadata {
  def getOrder(userName: String): Order[RoomMetadata] = Order.by { room =>
    val myRoles  = room.roleAssignments.filter(_._2 == userName).keySet
    val isMyTurn = myRoles.intersect(room.currentSpeakers).nonEmpty
    (!isMyTurn, room.currentParticipants.size, -room.latestUpdateTime)
  }
  def getOrdering(userName: String) = catsKernelOrderingForOrder(getOrder(userName))
}
