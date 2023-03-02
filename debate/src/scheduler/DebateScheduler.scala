package debate
package scheduler

import debate.util.SparseDistribution
import jjm.implicits._
import cats.implicits._

object DebateScheduler {

  def allAssignmentsForQuestion(
    storyId: SourceMaterialId,
    question: String,
    debaters: Set[String],
    numOfflineJudgesPerQuestion: Int
  ): Vector[Assignment] =
    for {
      honestDebater    <- debaters.toVector
      dishonestDebater <- debaters - honestDebater
      judge            <- debaters - honestDebater - dishonestDebater
      offlineJudges <- (debaters - honestDebater - dishonestDebater - judge)
        .toSeq
        .combinations(numOfflineJudgesPerQuestion)
        .map(_.toSet)
      honestFirst <- List(true, false)
      assignment = Assignment(
        storyId = storyId,
        question = question,
        honestDebater = honestDebater,
        judge = judge,
        dishonestDebater = dishonestDebater,
        offlineJudges = offlineJudges,
        honestFirst = honestFirst
      )
      if Constraints.isAssignmentValid(assignment)
    } yield assignment

  def allAssignmentsForStory(
    storyId: SourceMaterialId,
    questions: Vector[String],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    debaters: Set[String]
  ): Vector[Vector[Assignment]] =
    /** combinations are fine because we already know we don't want to exactly repeat a debate.
      */
    questions
      .traverse(question =>
        allAssignmentsForQuestion(storyId, question, debaters, numOfflineJudgesPerDebate)
          .toVector
          .combinations(numDebatesPerQuestion)
          .toVector
      )
      .map(_.flatten)

  def sample(probabilities: Vector[Double], rng: scala.util.Random): Int = {
    val randomDouble = rng.nextDouble()
    var sum          = 0.0
    for (i <- probabilities.indices) {
      sum += probabilities(i)
      if (randomDouble < sum) {
        return i
      }
    }
    return probabilities.length - 1
  }

  def zScores(x: Vector[Double]): Vector[Double] = {
    val mean = x.sum / x.length
    val stdDev = math.sqrt(
      x.map { xi =>
          math.pow(xi - mean, 2)
        }
        .sum / x.length
    )
    return x.map { xi =>
      (xi - mean) / stdDev
    }
  }

  // complete = debates.filter(_.isOver).flatMap(Assignment.fromDebate),
  // incomplete = debates.filterNot(_.isOver).flatMap(Assignment.fromDebate),
  def sampleScheduleForStory(
    complete: Vector[Assignment],
    incomplete: Vector[Assignment],
    storyId: SourceMaterialId,
    questions: Vector[String],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    debaters: Map[String, DebaterLoadConstraint], // TODO: change to or add soft constraints
    rng: scala.util.Random = scala.util.Random
  ): () => Schedule = {
    val workload = SparseDistribution(debaters.mapVals(_ => 1.0))
    // each vector in here is of length numQuestions
    val allSchedulesThatMeetConstraints = allAssignmentsForStory(
      storyId = storyId,
      questions = questions,
      numDebatesPerQuestion = numDebatesPerQuestion,
      numOfflineJudgesPerDebate = numOfflineJudgesPerDebate,
      debaters = debaters.keySet
    ).filter(Constraints.doesAssignmentObeyLoadConstraints(_, debaters))
      .map { newAssignments =>
        Schedule(
          desiredWorkload = workload,
          complete = complete,
          incomplete = incomplete,
          novel = newAssignments
        )
      }
      .filter(Constraints.doesScheduleMeetJudgingConstraints)

    val correspondingCosts = zScores(
      allSchedulesThatMeetConstraints.map {
        _.cost * -1
      }
    )
    val expCosts  = correspondingCosts.map(math.exp)
    val sumOfExps = expCosts.sum
    val probabilities = expCosts.map { expCost =>
      expCost / sumOfExps
    }
    return () => {
      val index = sample(probabilities, rng = rng)
      allSchedulesThatMeetConstraints(index)
    }
  }
}
