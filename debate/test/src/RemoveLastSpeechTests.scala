package debate

import munit.CatsEffectSuite

class RemoveLastSpeechTests extends CatsEffectSuite {
  // TODO what if two users submit a speech at the same time? what
  // should the undo button do? (granted it's extremely unlikely
  // that two users will submit a speech at the exact same timestamp,
  // but it's possible and reveals a confusion)
  test("SimultaneouSpeeches.removeLastSpeech") {
    val simultaneous = SimultaneousSpeeches(
      Map(
        0 -> DebateSpeech(0, 0, 0, 0),
        1 -> DebateSpeech(1, 1, 1, 1),
        2 -> DebateSpeech(2, 2, 2, 2)
      )
    )

    assert {
      simultaneous.removeLastSpeech == SimultaneousSpeeches(
        Map(
          0 -> DebateSpeech(0, 0, 0, 0),
          1 -> DebateSpeech(1, 1, 1, 1)
        )
      )
    }
  }

  // TODO test that it works for SequentialSpeeches
  // TODO test that it works for JudgeFeedback
}
