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
    println(assignments)
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
}
