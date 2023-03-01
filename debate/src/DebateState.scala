package debate

import cats.implicits._

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
    .result
    .map(result =>
      RoomStatus.Complete(result, debate.offlineJudgingResults, debate.feedback.keySet)
    )
    .getOrElse(
      if (debate.rounds.isEmpty)
        RoomStatus.WaitingToBegin
      else
        RoomStatus.InProgress
    )

  def metadata(roomName: String): RoomMetadata = RoomMetadata(
    name = roomName,
    sourceMaterialId = SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial),
    storyTitle = debate.setup.sourceMaterial.title,
    roleAssignments = debate.setup.roles,
    offlineJudgeAssignments = debate.setup.offlineJudges,
    creationTime = debate.setup.creationTime,
    status = status,
    latestUpdateTime = debate
      .rounds
      .view
      .flatMap(_.timestamp(debate.setup.numDebaters))
      .lastOption
      .getOrElse(debate.setup.creationTime),
    peopleWhoHaveSpoken = debate.rounds.foldMap(_.allSpeeches.values.view.map(_.speaker).toSet),
    currentSpeakers = debate.currentTransitions.currentSpeakers.flatMap(_.asLiveDebateRoleOpt),
    currentParticipants = participants.keySet
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
