package debate
package scheduler

import debate.util.SparseDistribution
import jjm.implicits._
import cats.implicits._

object DebateScheduler {
  // TODO is this a good value?
  val defaultJudgeScaleDownFactor = 0.3

  case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

  def isAssignmentValid(
    assignments: Vector[Assignment],
    constraints: Map[String, DebaterLoadConstraint]
  ): Boolean = constraints.forall { case (debater, constraint) =>
    val nParticipating = assignments.count(_.isAssigned(debater))
    constraint.min.forall(_ <= nParticipating) && constraint.max.forall(_ >= nParticipating)
  }

  def generateAllPossibleQuestionAssignments(
    storyId: SourceMaterialId,
    question: String,
    debaters: Set[String],
    numOfflineJudgesPerQuestion: Int
  ): Vector[Assignment] =
    // TODO someday add some validation for the strings in the debaters map and the history
    for {
      honestDebater    <- debaters.toVector
      dishonestDebater <- debaters - honestDebater
      judge            <- debaters - honestDebater - dishonestDebater
      offlineJudges <- (debaters - honestDebater - dishonestDebater - judge)
        .toSeq
        .combinations(numOfflineJudgesPerQuestion)
        .map(_.toSet)
      honestFirst <- List(true, false)
      dba = Assignment.create(
        storyId = storyId,
        question = question,
        honestDebater = honestDebater,
        judge = judge,
        dishonestDebater = dishonestDebater,
        offlineJudges = offlineJudges,
        honestFirst = honestFirst
      )
    } yield dba match {
      case Right(value) =>
        value
      case Left(exception) =>
        // an error shouldn't have happened here; seems like an internal error
        throw exception
    }

  def generateAllAssignments(
    storyId: SourceMaterialId,
    questions: Vector[String],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    debaters: Set[String]
  ): Vector[Vector[Assignment]] =
    /** TODO someday: notes from Julian
      * [combinations] seems fine for now, as repeating assignments in a single
      * round of scheduling seems very unlikely to be something we want, but
      * technically we might either:
      *
      * 1. be in the situation where an assignment needs to repeat in order to
      * balance things out (I think this is ok if it happens — after all,
      * they won't know it's happening, necessarily, or if they do, then
        it's probably intentional e.g. with load constraints), or
      * 
      * 2. we're scheduling more debates than there are possible assignments
      * (if we want to use for longer-term scheduling)
      */
    questions
      .traverse(question =>
        generateAllPossibleQuestionAssignments(
          storyId,
          question,
          debaters,
          numOfflineJudgesPerDebate
        ).toVector.combinations(numDebatesPerQuestion).toVector
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

  /**
     * Produces a list of assignments for a story. The story name can correspond to either a
     * new story or a story that has already been scheduled.
     * 
     * TODO XXX: fix these requirements.
     * 
     * Requirements:
     * - In the returned assignments, the debaters and judges are disjoint.
     * - Where DebaterLoadConstraint fields are present for a debater, they are obeyed (inclusive)
     * - To the extent possible, the following are (probabilistically) spread out as much as
     *   possible between people:
     *   - the number of stories each person has had to read
     *     (one must read a story to debate; cannot have read it to judge it)
     *   - the number of times each person has debated 
     *   - the number of times each person has judged each story
     *   - the number of times each person has judged (less important than judging per story)
     *   - the proportion of times each debater has been honest
     *   - the number of times each judge has judged each debater
     *   - the number of times each debater has debated each other
     *
     * @param history all existing debates, including unfinished ones
     * @param numQuestions the number of questions that can be debated for the new story
     * @param debaters the people who can be scheduled to judge or debate, with constraints on their load
     * @param rng the random number generator to use
     * @param storyName the name of the story to schedule for
     * @param judgeScaleDownFactor the factor by which to scale down the judge cost
     * @return a list of assignments of the debaters to their roles for the new story obeying the above requirements
     */
  def getScheduleForNewStory(
    history: Vector[Debate],
    storyId: SourceMaterialId,
    questions: Vector[String],
    numDebatesPerQuestion: Int,
    numOfflineJudgesPerDebate: Int,
    debaters: Map[String, DebaterLoadConstraint], // TODO: change to or add soft constraints
    rng: scala.util.Random = scala.util.Random
  ): Schedule = {
    val workload = SparseDistribution(debaters.mapVals(_ => 1.0))
    // each vector in here is of length numQuestions
    val allSchedulesThatMeetConstraints = generateAllAssignments(
      storyId = storyId,
      questions = questions,
      numDebatesPerQuestion = numDebatesPerQuestion,
      numOfflineJudgesPerDebate = numOfflineJudgesPerDebate,
      debaters = debaters.keySet
    ).filter(isAssignmentValid(_, debaters))
      .map { newAssignments =>
        Schedule(
          desiredWorkload = workload,
          complete = history.filter(_.isOver).flatMap(Assignment.fromDebate),
          incomplete = history.filterNot(_.isOver).flatMap(Assignment.fromDebate),
          novel = newAssignments
        )
      }

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
    val index = sample(probabilities, rng = rng)
    return allSchedulesThatMeetConstraints(index)
  }
}
