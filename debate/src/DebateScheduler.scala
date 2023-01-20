package debate

object DebateScheduler {
  case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])
  case class DebateAssignment(honestDebater: String, dishonestDebater: String, judge: String) {
    def isAssigned(debater: String): Boolean =
      honestDebater == debater || dishonestDebater == debater || judge == debater
  }

  // TODO should we rewrite the docstring for [getScheduleForNewStory] since we're not using probability?

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

  /** TODO 
   * 
   * 
   * potentially implement cost using something like this
   * 
   * 
   *
proportions_debater_has_been_honest = ...
expected_proportion_of_honesty = sum(proportions_has_been_honest) / len(proportions_has_been_honest)
cost += map(lambda x: abs(x - expected_proportion_of_honest), proportions_debater_has_been_honest) 

(this is a general notion of 'spread out ness')

this_cost = 0
for story in history:
  for debater in all_debaters:
    this_cost += abs(n_times_judging(debater, story) - (1 / n_debaters))
for debater in all_debaters:
  this_cost += abs(n_times_judging(debater, new_story) - (1 / n_debaters))
   * 
   * 
   */

  def getCost(assignment: Vector[DebateAssignment]): Int = {
    // one approach for how to implement is to compute a bunch of these values
    // (e.g. the proportion of times each debater has been honest)
    // and then compute a cost for that vector, measuring
    // how far each value is from the ideal value (where the ideal value is
    // avg(vector))
    val nStoriesSpreadFactor           = 1 // TODO how to implement, see below
    val nDebatesSpreadFactor           = 1 // TODO how to implement, see below
    val nJudgesSpreadFactor            = 1 // TODO how to implement, see below
    val nJudgesPerStorySpreadFactor    = 1 // TODO how to implement, see below
    val nHonestSpreadFactor            = 1 // TODO how to implement, see below
    val nJudgedPerDebaterSpreadFactor  = 1 // TODO how to implement, see below
    val nDebatedPerDebaterSpreadFactor = 1 // TODO how to implement, see below
    return nStoriesSpreadFactor + nDebatesSpreadFactor + nJudgesSpreadFactor +
      nJudgesPerStorySpreadFactor + nHonestSpreadFactor + nJudgedPerDebaterSpreadFactor +
      nDebatedPerDebaterSpreadFactor
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
     *     - the number of times each person has judged (less important than judging per story)
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
    val assignmentsSortedByCost: Vector[Vector[DebateAssignment]] =
      allAssignmentsThatMeetConstraints.sortBy { assignment =>
        // TODO someday amortize this so that it's faster
        getCost(assignment)
      }
    return assignmentsSortedByCost.head
  }
}
