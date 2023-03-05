package debate
package scheduler

import cats.implicits._

case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

object Constraints {
  import Schedule.DebateSetupExtensions

  // TODO: replace with global debatesetup validation

  def isSetupValid(setup: DebateSetup) =
    setup.allParticipants.size == (setup.offlineJudges.size + 3)

  def doesScheduleObeyLoadConstraints(
    schedule: Schedule,
    constraints: Map[String, DebaterLoadConstraint]
  ): Boolean = constraints.forall { case (debater, constraint) =>
    val nParticipating = schedule.allIncomplete.count(_.isAssigned(debater))
    constraint.min.forall(_ <= nParticipating) && constraint.max.forall(_ >= nParticipating)
  }

  def doesScheduleMeetJudgingConstraints(schedule: Schedule) = {
    val storyJudgingCounts = schedule
      .all
      .foldMap(a => a.judges.unorderedFoldMap(j => Map(j -> Map(a.storyId -> 1))))
    storyJudgingCounts.forall(_._2.forall(_._2 <= numJudgingsAllowedPerStory))
  }

}
