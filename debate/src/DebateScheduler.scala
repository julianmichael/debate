package debate

import cats.implicits._
import jjm.metrics.Numbers
import jjm.Duad
import jjm.implicits._

object DebateScheduler {
  // TODO is this a good value?
  val defaultJudgeScaleDownFactor = 0.3

  case class DebaterLoadConstraint(min: Option[Int], max: Option[Int])

  class DebateAssignment private (
    val honestDebater: String,
    val dishonestDebaters: Set[String],
    val judge: String
  ) {

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
    ): Either[Exception, DebateAssignment] = {
      if (honestDebater == judge)
        return Left(
          new IllegalArgumentException("Honest debater and judge cannot be the same person")
        )
      else if (dishonestDebaters.contains(judge))
        return Left(
          new IllegalArgumentException("Dishonest debaters and judge cannot be the same person")
        )
      else if (dishonestDebaters.contains(honestDebater))
        return Left(
          new IllegalArgumentException(
            s"Honest debater and dishonest debaters cannot be the same person (honest = $honestDebater, dishonest = ${dishonestDebaters
                .mkString(", ")})"
          )
        )
      Right(new DebateAssignment(honestDebater, dishonestDebaters, judge))
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
        debateAssignmentOrError = DebateAssignment(honestDebater, dishonestDebaters, judge)
      } yield debateAssignmentOrError match {
        case Right(assignment) =>
          assignment
        case Left(error) =>
          // this should never happen if the error comes from dishonest debater assignment, etc. , but technically it's possible in case there are new error cases
          throw error
      }
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

  def storiesReadCost(
    history: Vector[Debate],
    newAssignments: Vector[DebateAssignment],
    storyName: String
  ): Double = {
    // map of debater to set of stories read
    val debaterStoriesRead: Map[String, Set[String]] = history.foldMap { debate =>
      DebateAssignment
        .ofDebate(debate)
        .foldMap { assignment =>
          (assignment.dishonestDebaters + assignment.honestDebater)
            .view
            .map(debater => debater -> Set(debate.setup.sourceMaterial.title))
            .toMap
        }
    }

    // map of debater to set of stories read in new assignments
    // (each value should be a singleton set)
    val mapFromNewAssignments: Map[String, Set[String]] = newAssignments.foldMap { assignment =>
      (assignment.dishonestDebaters + assignment.honestDebater)
        .view
        .map(debater => debater -> Set(storyName))
        .toMap
    }

    // combine the sets if a debater is in both
    // |+| is the syntax for the Monoid#combine function, here from Monoid[Map[String, Set[String]]
    val combined: Map[String, Set[String]] = debaterStoriesRead |+| mapFromNewAssignments

    val storiesReadCounts: Map[String, Int] = combined.view.mapValues(_.size).toMap
    Numbers(storiesReadCounts.values.toVector).stats.stdev
  }

  def judgingPerStoryCost(
    history: Vector[Debate],
    newAssignments: Vector[DebateAssignment],
    storyName: String
  ): Double = {
    // TODO someday refactor
    val judgingInHistory: Map[String, Map[String, Int]] = history.foldMap { debate =>
      DebateAssignment
        .ofDebate(debate)
        .foldMap { assignment =>
          Map(debate.setup.sourceMaterial.title -> Map(assignment.judge -> 1))
        }
    }
    val judgingInNewAssignments: Map[String, Map[String, Int]] = Map(
      storyName -> newAssignments.map(_.judge).counts
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

  def debatedOtherDebatersCost(assignments: Vector[DebateAssignment]): Double = {
    val adversarialPairs: Map[Duad[String], Int] =
      assignments
        .foldMap { assignment =>
          val allDuads: Set[Duad[String]] = assignment
            .dishonestDebaters
            .map(assignment.honestDebater <-> _)
          allDuads.map(_ -> 1)
        }
        .toMap
    Numbers(adversarialPairs.values.toVector).stats.stdev
  }

  def judgedPerDebaterCost(assignments: Vector[DebateAssignment]): Double = {
    val judgeToDebaters: Map[String, Set[String]] =
      assignments
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

  def fractionsHonestWhenDebatingCost(assignments: Vector[DebateAssignment]): Double = {
    val timesHonest: Map[String, Int]    = assignments.map(_.honestDebater).counts
    val timesDishonest: Map[String, Int] = assignments.flatMap(_.dishonestDebaters).counts
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
  def getBadnessScore(
    newAssignments: Vector[DebateAssignment],
    history: Vector[Debate],
    judgeScaleDownFactor: Double,
    storyName: String
  ): Double = {
    val assignments = history.flatMap(DebateAssignment.ofDebate) ++ newAssignments
    val costParts = List(
      debaterCost(assignments), // doesn't depend on the story name
      storiesReadCost(history = history, newAssignments = newAssignments, storyName = storyName),
      judgeCost(assignments) * judgeScaleDownFactor,
      judgingPerStoryCost(
        history = history,
        newAssignments = newAssignments,
        storyName = storyName
      ),
      fractionsHonestWhenDebatingCost(assignments),
      judgedPerDebaterCost(assignments),
      debatedOtherDebatersCost(assignments)
    )
    costParts.sum
  }

  def generateAllPossibleQuestionAssignments(debaters: Set[String]): Iterable[DebateAssignment] =
    // TODO someday add some validation for the strings in the debaters map and the history
    for {
      honestDebater <- debaters
      judge         <- debaters - honestDebater
      if judge != honestDebater
      allDishonestDebaters = debaters.toSet - honestDebater - judge
      dishonestDebaters <- allDishonestDebaters.toSeq.combinations(debaters.size - 2).map(_.toSet)
      dba = DebateAssignment(
        honestDebater = honestDebater,
        judge = judge,
        dishonestDebaters = dishonestDebaters
      )
    } yield dba match {
      case Right(value) =>
        value
      case Left(exception) =>
        // an error shouldn't have happened here; seems like an internal error
        throw exception
    }

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
    debaters: Map[
      String,
      DebaterLoadConstraint
    ] // TODO someday ensure nonempty? so we can't return None?
    ,
    rng: scala.util.Random = scala.util.Random,
    storyName: String,
    judgeScaleDownFactor: Double = defaultJudgeScaleDownFactor
  ): Vector[DebateAssignment] = {
    // each vector in here is of length numQuestions
    val allAssignmentsThatMeetConstraints: Vector[Vector[DebateAssignment]] =
      generateAllAssignments(numQuestions = numQuestions, debaters = debaters).filter {
        assignment => isAssignmentValid(assignment, debaters)
      }
    val correspondingCosts = zScores(
      allAssignmentsThatMeetConstraints.map { newAssignments =>
        getBadnessScore(
          newAssignments = newAssignments,
          history = history,
          storyName = storyName,
          judgeScaleDownFactor = judgeScaleDownFactor
        ) * -1
      }
    )
    val expCosts  = correspondingCosts.map(math.exp)
    val sumOfExps = expCosts.sum
    val probabilities = expCosts.map { expCost =>
      expCost / sumOfExps
    }
    val index = sample(probabilities, rng = rng)
    return allAssignmentsThatMeetConstraints(index)
  }
}
