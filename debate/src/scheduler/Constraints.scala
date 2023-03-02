package debate
package scheduler

import cats.implicits._

case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

object Constraints {
  def isAssignmentValid(assignment: Assignment) =
    assignment.allParticipants.size == (assignment.offlineJudges.size + 3)

  def doesAssignmentObeyLoadConstraints(
    assignments: Vector[Assignment],
    constraints: Map[String, DebaterLoadConstraint]
  ): Boolean = constraints.forall { case (debater, constraint) =>
    val nParticipating = assignments.count(_.isAssigned(debater))
    constraint.min.forall(_ <= nParticipating) && constraint.max.forall(_ >= nParticipating)
  }

  def doesScheduleMeetJudgingConstraints(schedule: Schedule) = {
    val storyJudgingCounts = schedule
      .all
      .foldMap(a => a.judges.unorderedFoldMap(j => Map(j -> Map(a.storyId -> 1))))
    storyJudgingCounts.forall(_._2.forall(_._2 <= numJudgingsAllowedPerStory))
  }

}
