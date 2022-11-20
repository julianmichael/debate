package debate

import monocle.macros.Lenses
import io.circe.generic.JsonCodec

import cats.implicits._

/** The full data object maintained on the server for each debate. Sent to the
  * client in full each time there's a change to the debate.
  *
  * @param debate
  *   the contents of the debate
  * @param participants
  *   the people currently on the page (w/roles)
  */
@Lenses @JsonCodec case class DebateState(
    debate: Option[Debate],
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
  def init = DebateState(None, Set())
}

@Lenses @JsonCodec case class DebateResult(
  correctAnswerIndex: Int,
  numTurns: Int,
  finalJudgment: Vector[Double],
  judgeReward: Double
)
object DebateResult

/** The state of a debate. Persists when people leave; this is the saveable data
  * object.
  *
  * @param setup
  *   the debate's rules, structure, question and answer choices.
  * @param turns
  *   the sequence of arguments, feedback or other info exchanged in the debate.
  */
@Lenses @JsonCodec case class Debate(
    setup: DebateSetup,
    turns: Vector[DebateTurn]
) {

  def result: Option[DebateResult] = currentTurn.left.toOption
  def isOver: Boolean = result.nonEmpty
  def finalJudgment: Option[Vector[Double]] = result.map(_.finalJudgment)

  def numContinues = turns.foldMap {
    case JudgeFeedback(_, _, false) => 1
    case _ => 0
  }

  /** Whose turn it is and what they need to submit. */
  def currentTurn: Either[DebateResult, DebateTurnType] = {
    // TODO: validate that the debate follows the specified structure?
    // turn sequence is always nonempty
    val turnSequence = setup.rules.turnTypes(setup.answers.size)
    if (turns.isEmpty) Right(turnSequence.head)
    else {
      val lastTurnTypeAndRest = turnSequence.drop(turns.size - 1)
      val lastTurn = turns.last
      (lastTurnTypeAndRest.head, lastTurn) match {
        case (
              DebateTurnType.SimultaneousSpeechesTurn(debaters, charLimit),
              SimultaneousSpeeches(speeches)
            ) if speeches.size < debaters.size =>
          Right(
            DebateTurnType.SimultaneousSpeechesTurn(
              debaters -- speeches.keySet,
              charLimit
            )
          )
        case (_, JudgeFeedback(finalJudgment, _, true)) =>
          val numTurns = numContinues
          val judgeReward = setup.rules.scoringFunction.eval(
            numTurns, finalJudgment, setup.correctAnswerIndex
          )
          Left(
            DebateResult(
              correctAnswerIndex = setup.correctAnswerIndex,
              numTurns = numTurns,
              finalJudgment = finalJudgment,
              judgeReward = judgeReward
            )
          )
        case _ => Right(lastTurnTypeAndRest.tail.head) // should always be nonempty
      }
    }
  }
}
object Debate {
  def init(setup: DebateSetup): Debate = Debate(setup, Vector())
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
    feedback: DebateSpeech,
    endDebate: Boolean
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
    correctAnswerIndex: Int,
    startTime: Long
)
object DebateSetup
