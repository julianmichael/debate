package debate

import io.circe.generic.JsonCodec
import monocle.macros.GenPrism
import monocle.macros.Lenses

@JsonCodec
sealed trait SourceMaterial {
  def title: String
  def contents: Vector[String]
}
@JsonCodec
case class CustomSourceMaterial(title: String, contents: Vector[String]) extends SourceMaterial
@JsonCodec
case class QuALITYSourceMaterial(articleId: String, title: String, contents: Vector[String])
    extends SourceMaterial
object SourceMaterial {
  def quality = GenPrism[SourceMaterial, QuALITYSourceMaterial]
  def custom  = GenPrism[SourceMaterial, CustomSourceMaterial]
}

/** Info needed to set up a debate; what's set by the facilitator.
  *
  * @param rules
  *   the debate's rules/structure
  * @param sourceMaterial
  *   the source material visible to the debaters
  * @param question
  *   the question to be debated
  * @param answers
  *   all answer choices to be debated
  * @param startTime
  *   millis from epoch at which the debate was begun
  */
@Lenses
@JsonCodec
case class DebateSetup(
  rules: DebateRules,
  sourceMaterial: SourceMaterial,
  question: String,
  answers: Vector[String],
  correctAnswerIndex: Int,
  roles: Map[LiveDebateRole, String],
  offlineJudges: Map[String, Option[OfflineJudgingMode]],
  creationTime: Long
) {
  def numDebaters = answers.size

  def areAllRolesAssigned =
    roles.contains(Judge) && answers.indices.forall(i => roles.contains(Debater(i)))

  def assignedRoles(userName: String)  = roles.filter(_._2 == userName).keySet
  def userIsAssigned(userName: String) = assignedRoles(userName).nonEmpty
  def roleIsAssigned(role: Role) =
    role match {
      case role: LiveDebateRole =>
        roles.contains(role)
      case _ =>
        false
    }
  def canAssumeRole(userName: String, role: Role) =
    role.asLiveDebateRoleOpt.flatMap(roles.get) == Some(userName) ||
      (!roleIsAssigned(role) && !userIsAssigned(userName))
}
object DebateSetup {}
