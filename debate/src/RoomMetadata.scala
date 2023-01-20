package debate

import cats.implicits._
import cats.kernel.Order

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

@Lenses
@JsonCodec
case class RoomMetadata(
  name: String,
  storyTitle: String,
  roleAssignments: Map[DebateRole, String],
  creationTime: Long,
  status: RoomStatus,
  latestUpdateTime: Long,
  result: Option[DebateResult],
  currentSpeakers: Set[DebateRole],
  currentParticipants: Set[String]
) {
  def matchesQuery(query: String) = itemMatchesKeywordQuery(
    itemTerms = roleAssignments.values.toSet ++ currentParticipants + name + status.toString,
    queryKeywords = query.split("\\s+").toSet
  )

}
object RoomMetadata {
  def getOrder(userName: String): Order[RoomMetadata] = Order.by { room =>
    val myRoles  = room.roleAssignments.filter(_._2 == userName).keySet
    val isMyTurn = myRoles.intersect(room.currentSpeakers).nonEmpty
    (!isMyTurn, room.currentParticipants.size, -room.latestUpdateTime)
  }
  def getOrdering(userName: String) = catsKernelOrderingForOrder(getOrder(userName))
}
