package debate

import monocle.macros.Lenses
import io.circe.generic.JsonCodec
import cats.kernel.Order
import cats.implicits._

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
  private[this] def matchesKeyword(keyword: String) = {
    val k     = keyword.toLowerCase
    val words = roleAssignments.values.toSet ++ currentParticipants + name + status.toString
    words.exists(_.toLowerCase.contains(k))
  }
  def matchesQuery(query: String) = {
    val keywords = query.split("\\s+").toSet
    keywords.forall(matchesKeyword)
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
