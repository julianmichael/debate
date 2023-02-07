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
      assignments.size == numQuestions;
      assignments.forall { assignment =>
        assignment.honestDebater != assignment.judge &&
        !assignment.dishonestDebaters.contains(assignment.honestDebater)
        !assignment.dishonestDebaters.contains(assignment.judge)
      };
    }
  }

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
      rules = DebateRules.default,
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
    rounds = Vector.empty[DebateRound],
    offlineJudgingResults = Map(),
    feedback = Map()
  )

  test("the costs go down over time") {
    // the simplest cost to measure is debater cost
    var costs         = Vector.empty[Double]
    var history       = Vector.empty[Debate]
    var nTimesDebated = Vector.empty[Map[String, Int]]
    println("manually verify that this is almost sorted in decreasing order- these are the costs:")
    for (_ <- 1 to 5000) {
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
      history = history :+ testDebateOfAssignment(newAssignment.head)
      // TODO generalize this cost- this just counts the number of times a debater has been in a debate
      val thisCost = debaterCost(history.map(DebateAssignment.ofDebate).flatten)
      val thisN    = getNTimesDebated(history.map(DebateAssignment.ofDebate).flatten)
      costs = costs :+ thisCost
      nTimesDebated = nTimesDebated :+ thisN
      println(
        s"cost: $thisCost"
      ) // TODO maybe the cost should also use the z-score instead of the raw value
      println(s"nTimesDebated: $thisN")
    }
    assert {
      costs.head > costs.last
    }
  }
}
