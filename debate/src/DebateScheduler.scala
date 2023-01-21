package debate

object DebateScheduler {
  case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

  case class DebateAssignment(
    honestDebater: String,
    dishonestDebaters: Vector[String],
    judge: String
  ) {
    def isAssigned(debater: String): Boolean =
      honestDebater == debater || dishonestDebaters.contains(debater) || judge == debater
  }

  object DebateAssignment {

    /**
      * Returns the assignment of roles to users for a given debate.
      * Returns None if the debate has not been assigned roles.
      */
    def ofDebate(debate: Debate): Option[DebateAssignment] =
      for {
        honestDebater <- debate.honestDebaterAssignment
        dishonestDebaters = debate.dishonestDebatersAssignments
        judge <- debate.judgeAssignment
      } yield DebateAssignment(
        honestDebater = honestDebater,
        dishonestDebaters = dishonestDebaters,
        judge = judge
      )
  }

  def isAssignmentValid(
    assignment: Vector[DebateAssignment],
    debaters: Map[String, DebaterLoadConstraint]
  ): Boolean = {
    for (debater <- debaters.keys) {
      val constraint = debaters(debater)
      val nParticipanting = assignment.count {
        _.isAssigned(debater)
      }
      if (constraint.max.isDefined && constraint.max.get < nParticipanting) {
        return false
      }
      if (constraint.min.isDefined && constraint.min.get > nParticipanting) {
        return false
      }
    }
    return true
  }

  /** [assignments] is built from history and the new potential assignment */
  def debaterCost(assignments: Vector[DebateAssignment]): Double = {
    // TODO what do the indices correspond to- does it matter?
    val nTimesDebated: Vector[Int] = ???
    val stdDev = math.sqrt(
      nTimesDebated
        .map { n =>
          math.pow(n - 1, 2)
        }
        .sum / nTimesDebated.length
    )
    return stdDev
  }

  /** result is non-negative */
  def getBadnessScore(assignment: Vector[DebateAssignment]): Int = {
    var cost = 0
    val debaterCosts: Vector[Int] = timesDebated.map { debater =>
      // TODO get the terms cost/"badness score" straight

    }
    cost = cost + debaterCosts.sum()
    cost =
      cost +
        judgeCosts.sum() // TODO how to scale this down? (see the docstring for the main function)
    return cost
  }

  def generateAllAssignments(
    history: Vector[Debate],
    numQuestions: Int,
    debaters: Map[String, DebaterLoadConstraint]
  ): Iterable[Vector[DebateAssignment]] =
    for {
      honestDebater    <- debaters.keys
      dishonestDebater <- debaters.keys
      judge            <- debaters.keys
      // TODO when the spec says 'disjoint' does it mean 'no user is in more than one role'
      // should we *allow* users to be both the honest debater and the dishonest debater?
      if honestDebater != dishonestDebater && honestDebater != judge && dishonestDebater != judge &&
        true
    } yield Vector(
      DebateAssignment(
        honestDebater = honestDebater,
        dishonestDebater = dishonestDebater,
        judge = judge
      )
    )

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
    val allAssignments: Vector[Vector[DebateAssignment]] = generateAllAssignments(
      history = history,
      numQuestions = numQuestions,
      debaters = debaters
    )
    val allAssignmentsThatMeetConstraints: Vector[Vector[DebateAssignment]] = allAssignments
      .filter { assignment =>
        isAssignmentValid(assignment, debaters)
      }
    val correspondingCosts = allAssignmentsThatMeetConstraints.map { assignment =>
      getBadnessScore(assignment) * -1
    }
    val sumOfExps =
      correspondingCosts
        .map { cost =>
          math.exp(cost)
        }
        .sum
    val probabilities = correspondingCosts.map { cost =>
      math.exp(cost) / sumOfExps
    }
    val randomIndex = {
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
    return allAssignmentsThatMeetConstraints(randomIndex)
  }
}
