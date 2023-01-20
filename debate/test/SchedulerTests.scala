package debate

import munit.CatsEffectSuite

class SchedulerTests extends CatsEffectSuite {
  // TODO i dont think this is the right effect
  test("with no constraints, all debaters are assigned to all debates") {
    val debaters = Map(
      "debater1" -> DebaterLoadConstraint(None, None),
      "debater2" -> DebaterLoadConstraint(None, None),
      "debater3" -> DebaterLoadConstraint(None, None)
    )
    val numQuestions = 5
    val assignments = Scheduler.getScheduleForNewStory(
      history = Vector.empty,
      numQuestions = numQuestions,
      debaters = debaters
    )
    assert {
      // TODO type system
      assignments.size == numQuestions;
      assignments.forall { assignment =>
        assignment.honestDebater != assignment.dishonestDebater &&
        assignment.honestDebater != assignment.judge &&
        assignment.dishonestDebater != assignment.judge
      };
      assignments ==
        Vector(
          DebateAssignment("debater1", "debater2", "debater3"),
          DebateAssignment("debater1", "debater3", "debater2"),
          DebateAssignment("debater2", "debater1", "debater3"),
          DebateAssignment("debater2", "debater3", "debater1"),
          DebateAssignment("debater3", "debater1", "debater2"),
          DebateAssignment("debater3", "debater2", "debater1")
        )
    }
  }
}
