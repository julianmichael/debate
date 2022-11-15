package debate

import monocle.macros.Lenses


import io.circe.generic.JsonCodec

/** The full data object maintained on the server for each debate. Sent to the
  * client in full each time there's a change to the debate.
  *
  * @param debate
  *   the contents of the debate
  * @param participants
  *   the people currently on the page (w/roles)
  */
@Lenses @JsonCodec case class DebateState(
    debate: Debate,
    participants: Set[ParticipantId]
) {

  /** Add a participant. If the participant is already present, potentially
    * change their role.
    */
  def addParticipant(id: ParticipantId) = {
    copy(participants = participants.filter(_.name != id.name) + id)
  }
}
object DebateState {
  def init = DebateState(Debate.init, Set())
}

/** The state of a debate. Persists when people leave; this is the saveable data
  * object.
  *
  * @param setup
  *   the debate's rules, structure, question and answer choices.
  * @param turns
  *   the sequence of arguments, feedback or other info exchanged in the debate.
  */
@Lenses @JsonCodec case class Debate(
    setup: Option[DebateSetup],
    turns: Vector[DebateTurn]
) {

  /** Whose turn it is and what they need to submit. */
  def currentTurn: Option[DebateTurnType] = setup.map { setup =>
    // TODO: validate that the debate follows the specified structure?
    val turnSequence = setup.rules.turnTypes(setup.answers.size)
    if (turns.isEmpty) turnSequence.head
    else {
      val lastTurnTypeAndRest = turnSequence.drop(turns.size - 1)
      val lastTurn = turns.last
      (lastTurnTypeAndRest.head, lastTurn) match {
        case (
              DebateTurnType.SimultaneousSpeechesTurn(debaters, charLimit),
              SimultaneousSpeeches(speeches)
            ) if speeches.size < debaters.size =>
          DebateTurnType.SimultaneousSpeechesTurn(
            debaters -- speeches.keySet,
            charLimit
          )
        case _ => lastTurnTypeAndRest.tail.head
      }
    }
  }
}
object Debate {
  def init: Debate = Debate(None, Vector())
}

/** Outcome of a debate turn after some/all relevant parties have submitted
  * their arguments / info
  */
@JsonCodec sealed trait DebateTurn {
  def timestamp: Option[Long]
}
@Lenses @JsonCodec case class SimultaneousSpeeches(
    statements: Map[Int, DebateSpeech] // map from answer index -> statement
) extends DebateTurn {
  def timestamp = statements.values.toVector.map(_.timestamp).maxOption
}
object SimultaneousSpeeches
@Lenses @JsonCodec case class DebaterSpeech(
    speech: DebateSpeech
) extends DebateTurn {
  def timestamp = Some(speech.timestamp)
}
object DebaterSpeech
@Lenses @JsonCodec case class JudgeFeedback(
    distribution: Vector[Double], // probability distribution
    feedback: DebateSpeech
) extends DebateTurn {
  def timestamp = Some(feedback.timestamp)
}
object JudgeFeedback
object DebateTurn

/** Specifies who gets to speak next and what kind of input they should provide.
  */
sealed trait DebateTurnType {
  def charLimit: Int
}
object DebateTurnType {
  case class SimultaneousSpeechesTurn(
      remainingDebaters: Set[Int],
      charLimit: Int
  ) extends DebateTurnType
  case class DebaterSpeechTurn(debater: Int, charLimit: Int)
      extends DebateTurnType
  case class JudgeFeedbackTurn(reportBeliefs: Boolean, charLimit: Int)
      extends DebateTurnType
}

/** Info needed to set up a debate; what's set by the facilitator.
  *
  * @param rules
  *   the debate's rules/structure
  * @param sourceMaterial
  *   the source material visible to the debaters
  * @param question
  *   the question to be debated
  * @param answers
  *   all answer choices to be debated
  * @param startTime
  *   millis from epoch at which the debate was begun
  */
@Lenses @JsonCodec case class DebateSetup(
    rules: DebateRules,
    sourceMaterial: Vector[String],
    question: String,
    answers: Vector[String],
    startTime: Long
)
object DebateSetup
