package debate
package scheduler

import munit.CatsEffectSuite
// import DebateScheduler._

class SchedulerTests extends CatsEffectSuite {
  val debater1 = "debater1"
  val debater2 = "debater2"
  val debater3 = "debater3"

  def makeRandomStoryName(history: Vector[Debate]): String = {
    var randomNewStoryName = "test story"
    while (history.map(_.setup.sourceMaterial.title).contains(randomNewStoryName))
      randomNewStoryName = "test story " + scala.util.Random.nextInt(100)
    randomNewStoryName
  }

  val rng = new scala.util.Random

  val testSourceMaterial = CustomSourceMaterial(title = "test", contents = Vector.empty[String])

  // test("sampleScheduleForStory minimally works") {
  //   val debaters = Map(
  //     debater1 -> DebaterLoadConstraint(None, None),
  //     debater2 -> DebaterLoadConstraint(None, None),
  //     debater3 -> DebaterLoadConstraint(None, None)
  //   )
  //   val qas = (1 to 3)
  //     .map(i => QASpec(s"Question $i", correctAnswer = "Correct", incorrectAnswer = "Incorrect"))
  //     .toVector
  //   val schedule = getDebateScheduleDistribution(
  //     debates = Vector(),
  //     rules = DebateRules.default,
  //     sourceMaterial = testSourceMaterial,
  //     qas = qas,
  //     numDebatesPerQuestion = 2,
  //     numOfflineJudgesPerDebate = 0,
  //     debaters = debaters,
  //     creationTime = System.currentTimeMillis()
  //   ).get.sample(rng)

  //   import Schedule.DebateSetupExtensions

  //   assert {
  //     schedule.novel.size == qas.size;
  //     schedule
  //       .all
  //       .forall { setup =>
  //         setup.allParticipants.size == setup.offlineJudges.size + 3
  //       }
  //   }
  // }

  // the simplest cost to measure is debater cost
  // test("debater-only costs go down over time") {
  //   var costs         = Vector.empty[Double]
  //   var history       = Vector.empty[Debate]
  //   var nTimesDebated = Vector.empty[Map[String, Int]]
  //   for (_ <- 1 to 100) {
  //     val schedule = getScheduleForNewStory(
  //       history = history,
  //       numQuestions = 1,
  //       numDishonestDebatersPerQuestion = 1,
  //       numOfflineJudgesPerQuestion = 0,
  //       debaters = Map(
  //         debater1 -> DebaterLoadConstraint(None, None),
  //         debater2 -> DebaterLoadConstraint(None, None),
  //         debater3 -> DebaterLoadConstraint(None, None)
  //       ),
  //       storyId = SourceMaterialId.Custom(makeRandomStoryName(history = history))
  //     )
  //     assert {
  //       schedule.novel.size == 1
  //     }
  //     history = history :+ testDebateOfAssignment(schedule.novel.head)
  //     val thisCost = schedule.timesDebatedVariance // (history.map(Assignment.fromDebate).flatten)
  //     val thisN    = Schedule.numTimesDebating(history.map(Assignment.fromDebate).flatten)
  //     costs = costs :+ thisCost
  //     nTimesDebated = nTimesDebated :+ thisN
  //   }
  //   println(
  //     "manually verify that this looks right- debaters should be assigned roughly evenly, and the last cost should be pretty close to the first"
  //   )
  //   println("n times debated", nTimesDebated.last)
  //   println("costs.head", costs.head)
  //   println("costs.last", costs.last)
  //   println("costs", costs)
  // }

  // test("assignments using full costs are reasonable") {
  //   var costs   = Vector.empty[Double]
  //   var history = Vector.empty[Debate]
  //   for (_ <- 1 to 5) {
  //     val schedule =
  //       sampleScheduleForStory(
  //         complete = Vector(),
  //         incomplete = Vector(),
  //         questions = Vector("Question"),
  //         numDebatesPerQuestion = 1,
  //         numOfflineJudgesPerDebate = 0,
  //         debaters = Map(
  //           debater1 -> DebaterLoadConstraint(None, None),
  //           debater2 -> DebaterLoadConstraint(None, None),
  //           debater3 -> DebaterLoadConstraint(None, None)
  //         ),
  //         storyId = SourceMaterialId.Custom(makeRandomStoryName(history = history))
  //       )()
  //     assert {
  //       schedule.novel.size == 1
  //     }
  //     history = history :+ testDebateOfAssignment(schedule.novel.head)
  //     val thisCost = schedule.cost
  //     costs = costs :+ thisCost
  //   }
  //   println("for full cost measurement: " + costs)
  //   println(
  //     "for full cost measurement, all assignments: \n\n" +
  //       history.map(_.setup).map(Assignment.fromDebateSetup).map(o => o.map(_.toPrettyString))
  //   )
  // }
}
