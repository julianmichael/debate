package debate

import jjm.implicits._

object DebateScheduler {
  case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

  case class DebateAssignment(
    honestDebater: String,
    dishonestDebaters: Set[String],
    judge: String
  ) {
    def isAssigned(debater: String): Boolean =
      honestDebater == debater || dishonestDebaters.contains(debater) || judge == debater
  }

  object DebateAssignment {
    // Used in [ofDebate]
    private def honestDebaterAssignment(d: Debate): Option[String] = d
      .setup
      .roles
      .get(Debater(d.setup.correctAnswerIndex))

    // Used in [ofDebate]
    private def dishonestDebatersAssignments(d: Debate): Set[String] =
      d.setup
        .roles
        .keys
        .filter {
          case Debater(index) =>
            index != d.setup.correctAnswerIndex
          case _ =>
            false
        }
        .map { role =>
          d.setup.roles(role)
        }
        .toSet

    // Used in [ofDebate]
    private def judgeAssignment(d: Debate): Option[String] = d.setup.roles.get(Judge)

    /**
      * Returns the assignment of roles to users for a given debate.
      * Returns None if the debate has not been assigned roles.
      */
    def ofDebate(debate: Debate): Option[DebateAssignment] =
      for {
        honestDebater <- honestDebaterAssignment(debate)
        dishonestDebaters = dishonestDebatersAssignments(debate)
        judge <- judgeAssignment(debate)
      } yield DebateAssignment(
        honestDebater = honestDebater,
        dishonestDebaters = dishonestDebaters,
        judge = judge
      )
  }

  def isAssignmentValid(
    assignments: Vector[DebateAssignment],
    debaters: Map[String, DebaterLoadConstraint]
  ): Boolean = debaters.forall { case (debater, constraint) =>
    val nParticipating = assignments.count(_.isAssigned(debater))
    constraint.min.forall(_ <= nParticipating) && constraint.max.forall(_ >= nParticipating)
  }

  def getNTimesDebated(assignments: Vector[DebateAssignment]): Map[String, Int] =
    assignments
      .flatMap { assignment =>
        assignment.dishonestDebaters + assignment.honestDebater
      }
      .counts

  /** 
   * [assignments] is built from history and the new potential assignment.
   * 
   * this cost is the standard deviation of the number of times each debater has debated. (either as the honest or dishonest debater)
   * 
   */
  def debaterCost(assignments: Vector[DebateAssignment]): Double = {
    import jjm.metrics.Numbers
    val x = Numbers(getNTimesDebated(assignments).values.toVector).stats
    x.stdev
  }

  /** result is non-negative */
  def getBadnessScore(newAssignments: Vector[DebateAssignment], history: Vector[Debate]): Double = {
    var cost: Double = 0.0
    val assignments  = history.flatMap(DebateAssignment.ofDebate) ++ newAssignments
    cost = cost + debaterCost(assignments)
    /*
    cost =
      cost +
        judgeCosts.sum() // TODO how to scale this down? (see the docstring for the main function)
     */
    return cost
  }

  // TODO unit test this function
  def generateAllPossibleQuestionAssignments(debaters: Set[String]): Iterable[DebateAssignment] =
    // TODO someday add some validation for the strings in the debaters map and the history
    for {
      honestDebater <- debaters
      judge         <- debaters
      if honestDebater != judge
      allDishonestDebaters = debaters.toSet - honestDebater - judge
      dishonestDebaters <- allDishonestDebaters.toSeq.combinations(debaters.size - 2).map(_.toSet)
    } yield DebateAssignment(
      honestDebater = honestDebater,
      dishonestDebaters = dishonestDebaters,
      judge = judge
    )

  def generateAllAssignments(
    numQuestions: Int,
    debaters: Map[String, DebaterLoadConstraint]
  ): Vector[Vector[DebateAssignment]] = {

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
    val allPossibleQuestionAssignments = generateAllPossibleQuestionAssignments(debaters.keySet)
    allPossibleQuestionAssignments.toVector.combinations(numQuestions).toVector
  }

  def randomIndexOverProbabilities(probabilities: Vector[Double]): Int = {
    val randomDouble = scala.util.Random.nextDouble()
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
     * Produces a list of assignments for a new story.
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
     * @return a list of assignments of the debaters to their roles for the new story obeying the above requirements
     */
  def getScheduleForNewStory(
    history: Vector[Debate],
    numQuestions: Int,
    debaters: Map[String, DebaterLoadConstraint] // TODO ensure nonempty? so we can't return None?
  ): Vector[DebateAssignment] = {
    // each vector in here is of length numQuestions
    val allAssignmentsThatMeetConstraints: Vector[Vector[DebateAssignment]] =
      generateAllAssignments(numQuestions = numQuestions, debaters = debaters).filter {
        assignment => isAssignmentValid(assignment, debaters)
      }
    val correspondingCosts = zScores(
      allAssignmentsThatMeetConstraints.map { newAssignments =>
        getBadnessScore(newAssignments = newAssignments, history = history) * -1
      }
    )
    val sumOfExps =
      correspondingCosts
        .map { cost =>
          math.exp(cost)
        }
        .sum
    val probabilities = correspondingCosts.map { cost =>
      math.exp(cost) / sumOfExps
    }
    val index = randomIndexOverProbabilities(probabilities)
    return allAssignmentsThatMeetConstraints(index)
  }
}
