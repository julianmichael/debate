package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

@Lenses
@JsonCodec
case class Lobby(
  allDebaters: Set[String],
  trackedDebaters: Set[String],
  officialRooms: Set[RoomMetadata],
  practiceRooms: Set[RoomMetadata],
  leaderboard: Leaderboard
)
object Lobby {
  def empty = Lobby(Set(), Set(), Set(), Set(), Leaderboard(Map()))
}
