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

  def status: RoomStatus = debate match {
    case None => RoomStatus.SettingUp
    case Some(debate) => debate.currentTurn.fold(
      _ => RoomStatus.Complete, _ => RoomStatus.InProgress
    )
  }

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
    rounds: Vector[DebateRound]
) {

  def result: Option[DebateResult] = currentTurn.left.toOption
  def isOver: Boolean = result.nonEmpty
  def finalJudgment: Option[Vector[Double]] = result.map(_.finalJudgment)

  def numContinues = rounds.foldMap {
    case JudgeFeedback(_, _, false) => 1
    case _ => 0
  }

  /** Whose turn it is and what they need to submit. */
  def currentTurn: Either[DebateResult, DebateTurnType] = {
    // TODO: validate that the debate follows the specified structure?
    // turn sequence is always nonempty
    val numDebaters = setup.answers.size
    val roundSequence = setup.rules.roundTypes//(setup.answers.size)
    if (rounds.isEmpty) Right(roundSequence.head.getFirstTurn(numDebaters))
    else {
      val lastRoundTypeAndRest = roundSequence.drop(rounds.size - 1)
      val lastRound = rounds.last
      lastRoundTypeAndRest.head.getTurn(lastRound, numDebaters) match {
        case DebateTurnTypeResult.Next => Right(
          lastRoundTypeAndRest.tail.head.getFirstTurn(numDebaters) // there should always be more turns
        )
        case DebateTurnTypeResult.Turn(turn) => Right(turn)
        case DebateTurnTypeResult.End(finalJudgment) =>
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
        case DebateTurnTypeResult.Mismatch => ??? // TODO fail gracefully
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
@JsonCodec sealed trait DebateRound {
  def allSpeeches: Set[DebateSpeech]
  def isComplete(numDebaters: Int): Boolean
  final def timestamp: Option[Long] = allSpeeches.view.map(_.timestamp).maxOption
}
@Lenses @JsonCodec case class SimultaneousSpeeches(
    speeches: Map[Int, DebateSpeech] // map from answer index -> statement
) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches = speeches.values.toSet
}
object SimultaneousSpeeches
@Lenses @JsonCodec case class SequentialSpeeches(
    speeches: Map[Int, DebateSpeech]
) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches = speeches.values.toSet
}
object SequentialSpeeches
@Lenses @JsonCodec case class JudgeFeedback(
    distribution: Vector[Double], // probability distribution
    feedback: DebateSpeech,
    endDebate: Boolean
) extends DebateRound {
  def isComplete(numDebaters: Int) = true
  def allSpeeches = Set(feedback)
}
object JudgeFeedback
object DebateRound

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
    roles: Map[DebateRole, String],
    startTime: Long
) {
  def areAllRolesAssigned = {
    roles.contains(Judge) && answers.indices.forall(i => roles.contains(Debater(i)))
  }
}
object DebateSetup
