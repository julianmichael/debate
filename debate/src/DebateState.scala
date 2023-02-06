package debate

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

/** The full data object maintained on the server for each debate. Sent to the
  * client in full each time there's a change to the debate.
  *
  * @param debate
  *   the contents of the debate
  * @param participants
  *   the people currently on the page (w/roles)
  */
@Lenses
@JsonCodec
case class DebateState(debate: Debate, participants: Map[String, Role]) {

  def status: RoomStatus = debate
    .currentTransitions
    .fold(
      _ => RoomStatus.Complete,
      _ =>
        if (debate.rounds.isEmpty)
          RoomStatus.WaitingToBegin
        else
          RoomStatus.InProgress
    )

  /** Add a participant. If the participant is already present, potentially
    * change their role.
    */
  def addParticipant(name: String, role: Role) = copy(participants = participants + (name -> role))

  def canSwitchToRole(userName: String, role: Role) =
    debate.setup.canAssumeRole(userName, role) && !participants.get(userName).exists(_ == role)

}
object DebateState {
  def init(debate: Debate) = DebateState(debate, Map())
}
