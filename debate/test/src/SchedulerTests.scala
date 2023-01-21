package debate

import munit.CatsEffectSuite
import DebateScheduler._

class SchedulerTests extends CatsEffectSuite {
  val debater1 = "debater1"
  val debater2 = "debater2"
  val debater3 = "debater3"

  test("with no constraints, assignment works") {
    val debaters = Map(
      debater1 -> DebaterLoadConstraint(None, None),
      debater2 -> DebaterLoadConstraint(None, None),
      debater3 -> DebaterLoadConstraint(None, None)
    )
    val numQuestions = 5
    val assignments = getScheduleForNewStory(
      history = Vector.empty,
      numQuestions = numQuestions,
      debaters = debaters
    )
    assert {
      // TODO type system for encoding length
      assignments.size == numQuestions;
      assignments.forall { assignment =>
        assignment.honestDebater != assignment.judge &&
        !assignment.dishonestDebaters.contains(assignment.honestDebater)
        !assignment.dishonestDebaters.contains(assignment.judge)
      };
    }
  }

  // TODO probably export this to the debate rules object
  val testRules = DebateRules(
    scoringFunction = ScoringFunction.SphericalScoreWithLinearPenalty(0.5, 0.5),
    fixedClosing = None,
    fixedOpening = Vector.empty[DebateRoundType],
    repeatingStructure = Vector.empty[DebateRoundType],
    globalQuoteRestriction = None
  )

  val testSourceMaterial = CustomSourceMaterial(title = "test", contents = Vector.empty[String])

  def testDebateSetup(assignment: DebateAssignment): DebateSetup = {
    val correctAnswerIndex = 0
    val dishonestRoles: Map[DebateRole, String] =
      assignment.dishonestDebaters.map(Debater(1 + correctAnswerIndex) -> _).toMap
    val roles: Map[DebateRole, String] =
      dishonestRoles +
        (Debater(correctAnswerIndex) -> assignment.dishonestDebaters.head) +
        (Judge                       -> assignment.judge)

    DebateSetup(
      rules = testRules,
      sourceMaterial = testSourceMaterial,
      question = "test question",
      answers = Vector.empty[String],
      correctAnswerIndex = correctAnswerIndex,
      roles = roles,
      startTime = 0
    )

  }

  def testDebateOfAssignment(assignment: DebateAssignment) = Debate(
    setup = testDebateSetup(assignment),
    rounds = Vector.empty[DebateRound]
  )

  test("the costs go down over time") {
    // the simplest cost to measure is debater cost
    var costs   = Vector.empty[Double]
    var history = Vector.empty[Debate]
    for (_ <- 1 to 50) {
      val newAssignment = getScheduleForNewStory(
        history = history,
        numQuestions = 1,
        debaters = Map(
          debater1 -> DebaterLoadConstraint(None, None),
          debater2 -> DebaterLoadConstraint(None, None),
          debater3 -> DebaterLoadConstraint(None, None)
        )
      )
      assert {
        newAssignment.size == 1
      }
      // TODO lol right now the costs are increasing :)
      history = history :+ testDebateOfAssignment(newAssignment.head)
      val thisCost = debaterCost(history.map(DebateAssignment.ofDebate).flatten)
      costs = costs :+ thisCost
    }
    println("manually verify that this is almost sorted in decreasing order- these are the costs:")
    println(costs)
    assert {
      costs.head > costs.last
    }
  }
}
