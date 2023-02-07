package debate

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
  // TODO do we need to obey the constraints on the number of judges and dishonest debaters? (present in the debates in the history)

  def isAssignmentValid(
    assignments: Vector[DebateAssignment],
    debaters: Map[String, DebaterLoadConstraint]
  ): Boolean = debaters.forall { case (debater, constraint) =>
    val nParticipating = assignments.count(_.isAssigned(debater))
    constraint.min.forall(_ <= nParticipating) && constraint.max.forall(_ >= nParticipating)
  }

  def getNTimesDebated(assignments: Vector[DebateAssignment]): Map[String, Int] = assignments
    .flatMap { assignment =>
      assignment.dishonestDebaters + assignment.honestDebater
    }
    .groupBy { debater =>
      debater
    }
    .map { case (debater, assignments) =>
      (debater, assignments.length)
    }

  // TODO note in the pr that the commits aren't meaningful - feature level review :)

  /** 
   * [assignments] is built from history and the new potential assignment.
   * 
   * this cost is the standard deviation of the number of times each debater has debated. (either as the honest or dishonest debater)
   * 
   */
  def debaterCost(assignments: Vector[DebateAssignment]): Double = {
    val nTimesDebated: Map[String, Int] = getNTimesDebated(assignments)
    val totalN                          = nTimesDebated.values.sum
    val stdDev = math.sqrt(
      nTimesDebated
        .values
        .map { n =>
          math.pow(n - 1, 2)
        }
        .sum / totalN
    )
    return stdDev
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
      numQuestions = numQuestions,
      debaters = debaters
    )
    val allAssignmentsThatMeetConstraints: Vector[Vector[DebateAssignment]] = allAssignments
      .filter { assignment =>
        isAssignmentValid(assignment, debaters)
      }
    var correspondingCosts = allAssignmentsThatMeetConstraints.map { newAssignments =>
      getBadnessScore(newAssignments = newAssignments, history = history) * -1
    }
    val averageCost = correspondingCosts.sum / correspondingCosts.length
    val stdDevCost = math.sqrt(
      correspondingCosts
        .map { cost =>
          math.pow(cost - averageCost, 2)
        }
        .sum / correspondingCosts.length
    )
    correspondingCosts = correspondingCosts.map { cost =>
      (cost - averageCost) / stdDevCost // TODO i think z-score scales better
    }
    val sumOfExps =
      correspondingCosts
        .map { cost =>
          math.exp(cost)
        }
        .sum
    // println("costs", correspondingCosts)
    // println("sumOfExps", sumOfExps)
    val probabilities = correspondingCosts.map { cost =>
      math.exp(cost) / sumOfExps
    }
    // println("probabilities", probabilities)
    val index = randomIndexOverProbabilities(probabilities)
    return allAssignmentsThatMeetConstraints(index)
  }
}
