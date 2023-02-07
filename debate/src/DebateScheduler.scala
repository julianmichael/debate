package debate

import jjm.implicits._
import jjm.metrics.Numbers

object DebateScheduler {
  case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

  /** Don't use this directly, use [DebateAssignment.apply] instead */
  class DebateAssignment(
    val honestDebater: String,
    val dishonestDebaters: Set[String],
    val judge: String
  ) {

    /*

    val honestDebater: String,
    val dishonestDebaters: Set[String],
    val judge: String)

     */

    def isAssigned(debater: String): Boolean =
      honestDebater == debater || dishonestDebaters.contains(debater) || judge == debater

    def toPrettyString: String =
      s"""Honest debater: $honestDebater
         |Dishonest debaters: ${dishonestDebaters.mkString(", ")}
         |Judge: $judge""".stripMargin
  }

  object DebateAssignment {
    def apply(
      honestDebater: String,
      dishonestDebaters: Set[String],
      judge: String
    ): DebateAssignment = {
      if (honestDebater == judge)
        throw new IllegalArgumentException("Honest debater and judge cannot be the same person")
      if (dishonestDebaters.contains(judge))
        throw new IllegalArgumentException("Dishonest debaters and judge cannot be the same person")
      if (dishonestDebaters.contains(honestDebater))
        throw new IllegalArgumentException(
          s"Honest debater and dishonest debaters cannot be the same person (honest = $honestDebater, dishonest = ${dishonestDebaters
              .mkString(", ")})"
        )
      new DebateAssignment(honestDebater, dishonestDebaters, judge)
    }

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
  def debaterCost(assignments: Vector[DebateAssignment]): Double =
    Numbers(getNTimesDebated(assignments).values.toVector).stats.stdev

  def judgeCost(assignments: Vector[DebateAssignment]): Double =
    Numbers(assignments.map(_.judge).counts.values.toVector).stats.stdev

  def storiesRead(history: Vector[Debate], newAssignments: Vector[DebateAssignment]): Double = {
    val honestStoriesRead: Map[String, Set[String]] =
      history
        .flatMap { debate =>
          DebateAssignment
            .ofDebate(debate)
            .map { assignment =>
              assignment.honestDebater -> Set(debate.setup.sourceMaterial.title)
            }
        }
        .toMap
    // TODO someday refactor this, I got into a fight with scalac
    val dishonestDebatersToStory: Vector[(String, Set[String])] = history.flatMap { debate =>
      val dba: Option[DebateAssignment]  = DebateAssignment.ofDebate(debate)
      val dishonestDebaters: Set[String] = dba.map(_.dishonestDebaters).getOrElse(Set.empty)
      (debate.setup.sourceMaterial.title, dishonestDebaters) :: Nil
    }
    val dishonestStoriesRead: Map[String, Set[String]] =
      dishonestDebatersToStory
        .flatMap { case (title, dishonestDebaters) =>
          dishonestDebaters.map { debater =>
            debater -> Set(title)
          }
        }
        .toMap
    val countsInNewAssignment: Map[String, Int] =
      newAssignments
        .map { assignment =>
          assignment.honestDebater -> 1
        }
        .toMap ++
        newAssignments
          .flatMap { assignment =>
            assignment
              .dishonestDebaters
              .map { debater =>
                debater -> 1
              }
          }
          .toMap
    val combined: Map[String, Int] = (honestStoriesRead ++ dishonestStoriesRead)
      .view
      .mapValues(_.size)
      .toMap
    val storiesReadCounts: Map[String, Int] = combined ++ countsInNewAssignment
    Numbers(storiesReadCounts.values.toVector).stats.stdev
  }

  def judgingPerStory(history: Vector[Debate], newAssignments: Vector[DebateAssignment]): Double = {
    // TODO someday refactor
    val judgingInHistory: Map[String, Map[String, Int]] =
      history
        .flatMap { debate =>
          DebateAssignment
            .ofDebate(debate)
            .map { assignment =>
              debate.setup.sourceMaterial.title -> Map(assignment.judge -> 1)
            }
        }
        .toMap
    var randomKey = scala.util.Random.nextString(10)
    while (newAssignments.map(_.judge).contains(randomKey))
      randomKey = scala.util.Random.nextString(10)
    val judgingInNewAssignments: Map[String, Map[String, Int]] = Map(
      randomKey -> newAssignments.map(_.judge).counts
    )
    val storyToPersonToJudgeCount: Map[String, Map[String, Int]] =
      judgingInHistory ++ judgingInNewAssignments
    storyToPersonToJudgeCount
      .values
      .map { personToJudgeCount =>
        Numbers(personToJudgeCount.values.toVector).stats.stdev
      }
      .sum
  }

  def debatedOtherDebaters(assignments: Vector[DebateAssignment]): Double = {
    val adversarialPairs: Map[Set[String], Int] =
      assignments
        .flatMap { assignment =>
          val allSets: Set[Set[String]] = assignment
            .dishonestDebaters
            .map(Set(assignment.honestDebater, _))
          allSets.map(_ -> 1)
        }
        .toMap
    Numbers(adversarialPairs.values.toVector).stats.stdev
  }

  def judgedPerDebater(assignments: Vector[DebateAssignment]): Double = {
    val judgeToDebaters: Map[String, Set[String]] =
      assignments
        .map { assignment =>
          assignment.judge -> (Set(assignment.honestDebater) ++ assignment.dishonestDebaters)
        }
        .toMap
    // keys are tuples of (judge, debater)
    val judgedPerDebater: Map[(String, String), Int] =
      judgeToDebaters
        .flatMap { case (judge, debaters) =>
          debaters.map { debater =>
            (judge, debater) -> 1
          }
        }
        .toMap
    Numbers(judgedPerDebater.values.toVector).stats.stdev
  }

  def fractionsHonestWhenDebating(assignments: Vector[DebateAssignment]): Double = {
    val timesHonest: Map[String, Int]    = assignments.map(_.honestDebater).counts
    val timesDishonest: Map[String, Int] = assignments.flatMap(_.dishonestDebaters).counts
    val fractionsHonest: Map[String, Double] = timesHonest.map { case (debater, nHonest) =>
      val nDishonest = timesDishonest.getOrElse(debater, 0)
      debater -> (nHonest.toDouble / (nHonest + nDishonest))
    }
    Numbers(fractionsHonest.values.toVector).stats.stdev
  }

  /** result is non-negative */
  def getBadnessScore(
    newAssignments: Vector[DebateAssignment],
    history: Vector[Debate],
    judgeScaleDownFactor: Double = 0.3 // TODO is this a good value?
  ): Double = {
    var cost: Double = 0.0
    val assignments  = history.flatMap(DebateAssignment.ofDebate) ++ newAssignments
    cost = cost + debaterCost(assignments)
    cost = cost + storiesRead(history = history, newAssignments = newAssignments)
    cost = cost + (judgeCost(assignments) * judgeScaleDownFactor)
    cost = cost + judgingPerStory(history = history, newAssignments = newAssignments)
    cost = cost + fractionsHonestWhenDebating(assignments)
    cost = cost + judgedPerDebater(assignments)
    cost = cost + debatedOtherDebaters(assignments)
    return cost
  }

  def generateAllPossibleQuestionAssignments(debaters: Set[String]): Iterable[DebateAssignment] =
    // TODO someday add some validation for the strings in the debaters map and the history
    for {
      honestDebater <- debaters
      judge         <- debaters - honestDebater
      if judge != honestDebater
      allDishonestDebaters = debaters.toSet - honestDebater - judge
      dishonestDebaters <- allDishonestDebaters.toSeq.combinations(debaters.size - 2).map(_.toSet)
    } yield DebateAssignment(
      honestDebater = honestDebater,
      judge = judge,
      dishonestDebaters = dishonestDebaters
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

  def sample(probabilities: Vector[Double], rng: scala.util.Random = scala.util.Random): Int = {
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
    debaters: Map[
      String,
      DebaterLoadConstraint
    ] // TODO someday ensure nonempty? so we can't return None?
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
    val index = sample(probabilities)
    return allAssignmentsThatMeetConstraints(index)
  }
}
