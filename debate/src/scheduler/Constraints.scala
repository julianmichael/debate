package debate
package scheduler

import cats.implicits._

import debate.numJudgingsAllowedPerStory

case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

object Constraints {
  import Schedule.DebateSetupExtensions

  def doesScheduleObeyLoadConstraints(
    schedule: Schedule,
    constraints: Map[String, DebaterLoadConstraint]
  ): Boolean = constraints.forall { case (debater, constraint) =>
    val nParticipating = schedule.allIncomplete.count(_.isAssigned(debater))
    constraint.min.forall(_ <= nParticipating) && constraint.max.forall(_ >= nParticipating)
  }

  def doesScheduleMeetJudgingConstraints(schedule: Schedule) = {
    val priorStoryJudgingCounts = schedule
      .copy(novel = Vector())
      .all
      .foldMap(a => a.judges.unorderedFoldMap(j => Map(j -> Map(a.storyId -> 1))))
    val storyJudgingCounts = schedule
      .all
      .foldMap(a => a.judges.unorderedFoldMap(j => Map(j -> Map(a.storyId -> 1))))
    storyJudgingCounts.forall { case (judge, storyCounts) =>
      storyCounts.forall { case (story, count) =>
        // violations are only allowed if they were already in the schedule
        (count > numJudgingsAllowedPerStory) -->
          priorStoryJudgingCounts.get(judge).exists(_.get(story).exists(_ >= count))
      }
    }
  }

}
