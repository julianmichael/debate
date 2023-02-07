package debate

import cats.implicits._
import cats.kernel.Order

import io.circe.generic.JsonCodec
import monocle.macros.Lenses
import monocle.macros.GenPrism

@JsonCodec
sealed trait SourceMaterialId
object SourceMaterialId {
  case class QuALITYStory(id: String) extends SourceMaterialId
  case class Custom(title: String)    extends SourceMaterialId
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

}
object RoomMetadata {
  def getOrder(userName: String): Order[RoomMetadata] = Order.by { room =>
    val myRoles  = room.roleAssignments.filter(_._2 == userName).keySet
    val isMyTurn = myRoles.intersect(room.currentSpeakers).nonEmpty
    (!isMyTurn, room.currentParticipants.size, -room.latestUpdateTime)
  }
  def getOrdering(userName: String) = catsKernelOrderingForOrder(getOrder(userName))
}
