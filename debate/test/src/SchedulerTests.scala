package debate

import munit.CatsEffectSuite
import DebateScheduler._

class SchedulerTests extends CatsEffectSuite {
  val debater1 = "debater1"
  val debater2 = "debater2"
  val debater3 = "debater3"

  // TODO someday-maybe make this a method on a history type :)
  def makeRandomStoryName(history: Vector[Debate]): String = {
    var randomNewStoryName = "test story"
    while (history.map(_.setup.sourceMaterial.title).contains(randomNewStoryName))
      randomNewStoryName = "test story " + scala.util.Random.nextInt(100)
    randomNewStoryName
  }

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
      debaters = debaters,
      storyName = makeRandomStoryName(history = Vector.empty)
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
        (Debater(correctAnswerIndex) -> assignment.honestDebater) +
        (Judge                       -> assignment.judge)

    DebateSetup(
      rules = DebateRules.default,
      sourceMaterial = testSourceMaterial,
      question = "test question",
      answers = Vector("the answer to everything, trust me", "the answer to nothing, trust me"),
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

  // the simplest cost to measure is debater cost
  test("debater-only costs go down over time") {
    var costs         = Vector.empty[Double]
    var history       = Vector.empty[Debate]
    var nTimesDebated = Vector.empty[Map[String, Int]]
    for (_ <- 1 to 100) {
      val newAssignment = getScheduleForNewStory(
        history = history,
        numQuestions = 1,
        debaters = Map(
          debater1 -> DebaterLoadConstraint(None, None),
          debater2 -> DebaterLoadConstraint(None, None),
          debater3 -> DebaterLoadConstraint(None, None)
        ),
        storyName = makeRandomStoryName(history = history)
      )
      assert {
        newAssignment.size == 1
      }
      history = history :+ testDebateOfAssignment(newAssignment.head)
      val thisCost = debaterCost(history.map(DebateAssignment.ofDebate).flatten)
      val thisN    = getNTimesDebated(history.map(DebateAssignment.ofDebate).flatten)
      costs = costs :+ thisCost
      nTimesDebated = nTimesDebated :+ thisN
    }
    println(
      "manually verify that this looks right- debaters should be assigned roughly evenly, and the last cost should be pretty close to the first"
    )
    println("n times debated", nTimesDebated.last)
    println("costs.head", costs.head)
    println("costs.last", costs.last)
    println("costs", costs)
  }

  test("assignments using full costs are reasonable") {
    var costs   = Vector.empty[Double]
    var history = Vector.empty[Debate]
    for (_ <- 1 to 5) {
      val newAssignment = getScheduleForNewStory(
        history = history,
        numQuestions = 1,
        debaters = Map(
          debater1 -> DebaterLoadConstraint(None, None),
          debater2 -> DebaterLoadConstraint(None, None),
          debater3 -> DebaterLoadConstraint(None, None)
        ),
        storyName = makeRandomStoryName(history = history)
      )
      assert {
        newAssignment.size == 1
      }
      history = history :+ testDebateOfAssignment(newAssignment.head)
      val thisCost = getBadnessScore(
        history = history,
        newAssignments = newAssignment,
        storyName = makeRandomStoryName(history = history),
        judgeScaleDownFactor = defaultJudgeScaleDownFactor
      )
      costs = costs :+ thisCost
    }
    println("for full cost measurement: " + costs)
    println(
      "for full cost measurement, all assignments: \n\n" +
        history.map(DebateAssignment.ofDebate).map(o => o.map(_.toPrettyString))
    )
  }
}
