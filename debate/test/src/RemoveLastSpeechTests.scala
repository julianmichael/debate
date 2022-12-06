package debate

import munit.CatsEffectSuite

class RemoveLastSpeechTests extends CatsEffectSuite {
  val judge = ParticipantId("Alice", Judge)
  val firstDebater = ParticipantId("Bob", Debater(0))
  val secondDebater = ParticipantId("Charlie", Debater(1))

  val participants: Set[ParticipantId] = Set(
    judge,
    firstDebater,
    secondDebater
  )

  val setup: DebateSetup = DebateSetup(
    rules =  DebateRules(Vector(), Vector(), ScoringFunction.),
    sourceMaterial  = SourceMaterial,
    question  = String,
    answers  = Vector[String],
    correctAnswerIndex  = Int,
    roles  = Map[DebateRole, String],
    startTime  = Long
  )

  val judgeRound: DebateRound = JudgeFeedback(
    distribution = Vector(0.5, 0.5),
    feedback = DebateSpeech(
      speaker = judge,
      timestamp = 0,
      content = Vector() // no need for text
    ),
    endDebate = false
  )

  // TODO what if two users submit a speech at the same time? what
  // should the undo button do? (granted it's extremely unlikely
  // that two users will submit a speech at the exact same timestamp,
  // but it's possible and reveals a confusion)
  test("after the opening round, only judge can undo") {
    val debate = Debate(setup = setup, rounds = Vector(judgeRound))
    assert { debate.canUndo() == Set(judge) }
  }
}

// TODO test that it works for SequentialSpeeches
