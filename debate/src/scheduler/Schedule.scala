package debate
package scheduler

import cats.implicits._
import jjm.metrics.Numbers
import jjm.Duad
import jjm.implicits._

case class Schedule(
  complete: Vector[Assignment],
  incomplete: Vector[Assignment],
  novel: Vector[Assignment]
) {
  val allIncomplete = incomplete ++ novel
  val all           = complete ++ allIncomplete

  import Schedule._

  // TODO: these are probably not ideal.
  // instead, score will be by deviation from desired load.
  def timesDebatedVariance = Numbers(numTimesDebating(all).values.toVector).stats.variance

  def timesJudgedVariance = Numbers(numTimesJudging(all).values.toVector).stats.variance

  def storiesReadCost: Double = {
    val storiesRead = all.foldMap { assignment =>
      assignment.debaters.view.map(_ -> Set(assignment.storyId)).toMap
    }
    Numbers(storiesRead.values.view.map(_.size).toVector).stats.stdev
  }

  // TODO: probably want to just do sum square counts to spread them out
  def judgingPerStory: Double = {
    val judging = all.foldMap { assignment =>
      Map(assignment.storyId -> Map(assignment.judge -> 1))
    }
    judging
      .values
      .map { personToJudgeCount =>
        Numbers(personToJudgeCount.values.toVector).stats.stdev
      }
      .sum
  }

  def debatedOtherDebaters: Double = {
    val adversarialPairs: Map[Duad[String], Int] =
      all
        .foldMap { assignment =>
          val allDuads: Set[Duad[String]] = assignment
            .dishonestDebaters
            .map(assignment.honestDebater <-> _)
          allDuads.map(_ -> 1)
        }
        .toMap
    Numbers(adversarialPairs.values.toVector).stats.stdev
  }

  def judgedPerDebater: Double = {
    val judgeToDebaters: Map[String, Set[String]] =
      all
        .map { assignment =>
          assignment.judge -> (Set(assignment.honestDebater) ++ assignment.dishonestDebaters)
        }
        .toMap
    // keys are tuples of (judge, debater)
    val judgedPerDebater: Map[(String, String), Int] = judgeToDebaters
      .toSeq
      .foldMap { case (judge, debaters) =>
        debaters
          .toSeq
          .foldMap { debater =>
            Map((judge, debater) -> 1)
          }
      }

    Numbers(judgedPerDebater.values.toVector).stats.stdev
  }

  def fractionsHonestWhenDebating: Double = {
    val timesHonest: Map[String, Int]    = all.map(_.honestDebater).counts
    val timesDishonest: Map[String, Int] = all.flatMap(_.dishonestDebaters).counts
    val debaters                         = timesHonest.keySet ++ timesDishonest.keySet
    val fractionsHonest =
      debaters
        .view
        .map { debater =>
          val nHonest    = timesHonest.getOrElse(debater, 0)
          val nDishonest = timesDishonest.getOrElse(debater, 0)
          debater -> (nHonest.toDouble / (nHonest + nDishonest))
        }
        .toMap
    Numbers(fractionsHonest.values.toVector).stats.stdev
  }

  /** result is non-negative */
  def cost(judgeScaleDownFactor: Double): Double = {
    val costParts = List(
      timesDebatedVariance, // doesn't depend on the story name
      storiesReadCost,
      timesJudgedVariance * judgeScaleDownFactor,
      judgingPerStory,
      fractionsHonestWhenDebating,
      judgedPerDebater,
      debatedOtherDebaters
    )
    costParts.sum
  }
}
object Schedule {

  def numTimesDebating(assignments: Vector[Assignment]): Map[String, Int] =
    assignments
      .view
      .flatMap { assignment =>
        assignment.dishonestDebaters + assignment.honestDebater
      }
      .toVector
      .counts

  def numTimesJudging(assignments: Vector[Assignment]): Map[String, Int] =
    assignments
      .view
      .flatMap { assignment =>
        assignment.offlineJudges + assignment.judge
      }
      .toVector
      .counts
}
