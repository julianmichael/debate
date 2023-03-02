package debate
package scheduler

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
    val schedule = getScheduleForNewStory(
      history = Vector.empty,
      numQuestions = numQuestions,
      numDishonestDebatersPerQuestion = 1,
      numOfflineJudgesPerQuestion = 0,
      debaters = debaters,
      storyId = SourceMaterialId.Custom(makeRandomStoryName(history = Vector.empty))
    )
    assert {
      schedule.novel.size == numQuestions;
      schedule
        .all
        .forall { assignment =>
          assignment.honestDebater != assignment.judge &&
          !assignment.dishonestDebaters.contains(assignment.honestDebater)
          !assignment.dishonestDebaters.contains(assignment.judge)
        };
    }
  }

  val testSourceMaterial = CustomSourceMaterial(title = "test", contents = Vector.empty[String])

  def testDebateSetup(assignment: Assignment): DebateSetup = {
    val correctAnswerIndex = 0
    val dishonestRoles: Map[LiveDebateRole, String] =
      assignment.dishonestDebaters.map(Debater(1 + correctAnswerIndex) -> _).toMap
    val roles: Map[LiveDebateRole, String] =
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
      offlineJudges = Map(),
      creationTime = 0
    )
  }

  def testDebateOfAssignment(assignment: Assignment) = Debate(
    setup = testDebateSetup(assignment),
    rounds = Vector.empty[DebateRound],
    feedback = Map()
  )

  // the simplest cost to measure is debater cost
  test("debater-only costs go down over time") {
    var costs         = Vector.empty[Double]
    var history       = Vector.empty[Debate]
    var nTimesDebated = Vector.empty[Map[String, Int]]
    for (_ <- 1 to 100) {
      val schedule = getScheduleForNewStory(
        history = history,
        numQuestions = 1,
        numDishonestDebatersPerQuestion = 1,
        numOfflineJudgesPerQuestion = 0,
        debaters = Map(
          debater1 -> DebaterLoadConstraint(None, None),
          debater2 -> DebaterLoadConstraint(None, None),
          debater3 -> DebaterLoadConstraint(None, None)
        ),
        storyId = SourceMaterialId.Custom(makeRandomStoryName(history = history))
      )
      assert {
        schedule.novel.size == 1
      }
      history = history :+ testDebateOfAssignment(schedule.novel.head)
      val thisCost = schedule.timesDebatedVariance // (history.map(Assignment.fromDebate).flatten)
      val thisN    = Schedule.numTimesDebating(history.map(Assignment.fromDebate).flatten)
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
      val schedule = getScheduleForNewStory(
        history = history,
        numQuestions = 1,
        numDishonestDebatersPerQuestion = 1,
        numOfflineJudgesPerQuestion = 0,
        debaters = Map(
          debater1 -> DebaterLoadConstraint(None, None),
          debater2 -> DebaterLoadConstraint(None, None),
          debater3 -> DebaterLoadConstraint(None, None)
        ),
        storyId = SourceMaterialId.Custom(makeRandomStoryName(history = history))
      )
      assert {
        schedule.novel.size == 1
      }
      history = history :+ testDebateOfAssignment(schedule.novel.head)
      val thisCost = schedule.cost(judgeScaleDownFactor = defaultJudgeScaleDownFactor)
      costs = costs :+ thisCost
    }
    println("for full cost measurement: " + costs)
    println(
      "for full cost measurement, all assignments: \n\n" +
        history.map(Assignment.fromDebate).map(o => o.map(_.toPrettyString))
    )
  }
}