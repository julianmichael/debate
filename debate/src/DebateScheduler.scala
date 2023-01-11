package debate

object DebateScheduler {
  case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])
  case class DebateAssignment(honestDebater: String, dishonestDebater: String, judge: String)

  def getCost(assignment: Vector[DebateAssignment]): Int = {
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
      if honestDebater != dishonestDebater && honestDebater != judge &&
        dishonestDebater != judge && {
          // TODO ensure we're obeying the load constraints
          true
        }
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
    var lowestCost = Int.MaxValue
    // each vector in here is of length numQuestions
    val allAssignments: Vector[Vector[DebateAssignment]] = generateAllAssignments(
      history,
      numQuestions
    )
    val allAssignmentsThatMeetConstraints: Vector[Vector[DebateAssignment]] = allAssignments
      .filter { assignment =>
        // TODO ensure we're obeying the load constraints
        true
      }
    val assignmentsSortedByCost: Vector[Vector[DebateAssignment]] =
      allAssignmentsThatMeetConstraints.sortBy { assignment =>
        getCost(assignment) // TODO maybe amortize this so that it's faster
      }
    return assignmentsSortedByCost.head
  }
}
