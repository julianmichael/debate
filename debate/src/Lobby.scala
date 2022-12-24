package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

@Lenses
@JsonCodec
case class Lobby(
  trackedDebaters: Set[String],
  officialRooms: Vector[RoomMetadata],
  practiceRooms: Vector[RoomMetadata],
  leaderboard: Leaderboard
)
object Lobby {
  def init = Lobby(Set(), Vector(), Vector(), Leaderboard(Map()))
}
