package debate
package scheduler

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
    debaters: Set[String],
    numDishonestDebatersPerQuestion: Int,
    numOfflineJudgesPerQuestion: Int
  ): Iterable[Assignment] =
    // TODO someday add some validation for the strings in the debaters map and the history
    for {
      honestDebater <- debaters
      judge         <- debaters - honestDebater
      if judge != honestDebater
      allPossibleDishonestDebaters = debaters.toSet - honestDebater - judge
      dishonestDebaters <- allPossibleDishonestDebaters
        .toSeq
        .combinations(numDishonestDebatersPerQuestion)
        .map(_.toSet)
      allPossibleOfflineJudges = allPossibleDishonestDebaters -- dishonestDebaters
      offlineJudges <- allPossibleOfflineJudges
        .toSeq
        .combinations(numOfflineJudgesPerQuestion)
        .map(_.toSet)
      dba = Assignment.create(
        storyId = storyId,
        honestDebater = honestDebater,
        judge = judge,
        dishonestDebaters = dishonestDebaters,
        offlineJudges = offlineJudges
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
    numQuestions: Int,
    numDishonestDebatersPerQuestion: Int,
    numOfflineJudgesPerQuestion: Int,
    debaters: Map[String, DebaterLoadConstraint]
  ): Vector[Vector[Assignment]] = {

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
    val allPossibleQuestionAssignments = generateAllPossibleQuestionAssignments(
      storyId,
      debaters.keySet,
      numDishonestDebatersPerQuestion,
      numOfflineJudgesPerQuestion
    )
    allPossibleQuestionAssignments.toVector.combinations(numQuestions).toVector
  }

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
    numQuestions: Int,
    numDishonestDebatersPerQuestion: Int,
    numOfflineJudgesPerQuestion: Int,
    debaters: Map[
      String,
      DebaterLoadConstraint
    ] // TODO someday ensure nonempty? so we can't return None?
    ,
    rng: scala.util.Random = scala.util.Random,
    storyId: SourceMaterialId,
    judgeScaleDownFactor: Double = defaultJudgeScaleDownFactor
  ): Schedule = {
    // each vector in here is of length numQuestions
    val allSchedulesThatMeetConstraints = generateAllAssignments(
      storyId = storyId,
      numQuestions = numQuestions,
      numDishonestDebatersPerQuestion = numDishonestDebatersPerQuestion,
      numOfflineJudgesPerQuestion = numOfflineJudgesPerQuestion,
      debaters = debaters
    ).filter { assignment =>
        isAssignmentValid(assignment, debaters)
      }
      .map { newAssignments =>
        Schedule(
          complete = history.filter(_.isOver).flatMap(Assignment.fromDebate),
          incomplete = history.filterNot(_.isOver).flatMap(Assignment.fromDebate),
          novel = newAssignments
        )
      }

    val correspondingCosts = zScores(
      allSchedulesThatMeetConstraints.map {
        _.cost(judgeScaleDownFactor = judgeScaleDownFactor) * -1
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
