package livechat

import monocle.function.{all => Optics}
import monocle.std.{all => StdOptics}
import monocle.macros.Lenses

import jjm.Dot
import jjm.ling.TokenText
import jjm.ling.ESpan

import io.circe.generic.JsonCodec
import cats.data.NonEmptyList
import monocle.macros.GenPrism

// schema for round types that you specify to set up the debate
@JsonCodec sealed trait DebateRoundType {
  import DebateRoundType._
  def getTurns(numDebaters: Int): Vector[DebateTurnType] = this match {
    case SimultaneousSpeechesRound(charLimit) => Vector(DebateTurnType.SimultaneousSpeechesTurn((0 until numDebaters).toSet, charLimit))
    case SequentialSpeechesRound(charLimit) => (0 until numDebaters).map(i => DebateTurnType.DebaterSpeechTurn(i, charLimit)).toVector
    case JudgeFeedbackRound(reportBeliefs, charLimit) => Vector(DebateTurnType.JudgeFeedbackTurn(reportBeliefs, charLimit))
  }
}
object DebateRoundType {
  @Lenses @JsonCodec case class SimultaneousSpeechesRound(charLimit: Int) extends DebateRoundType
  val simultaneousSpeechesRound = GenPrism[DebateRoundType, SimultaneousSpeechesRound]
  @Lenses @JsonCodec case class SequentialSpeechesRound(charLimit: Int) extends DebateRoundType
  val sequentialSpeechesRound = GenPrism[DebateRoundType, SequentialSpeechesRound]
  @Lenses @JsonCodec case class JudgeFeedbackRound(reportBeliefs: Boolean, charLimit: Int) extends DebateRoundType
  val judgeFeedbackRound = GenPrism[DebateRoundType, JudgeFeedbackRound]
}

// object specifying who gets to speak next and what kind of input they should provide
sealed trait DebateTurnType {
  def charLimit: Int
}
object DebateTurnType {
  case class SimultaneousSpeechesTurn(remainingDebaters: Set[Int], charLimit: Int) extends DebateTurnType
  case class DebaterSpeechTurn(debater: Int, charLimit: Int) extends DebateTurnType
  case class JudgeFeedbackTurn(reportBeliefs: Boolean, charLimit: Int) extends DebateTurnType
}

// Outcome of a debate turn after some/all relevant parties have submitted their arguments / info
@JsonCodec sealed trait DebateTurn {
  def timestamp: Option[Long]
}
@Lenses @JsonCodec case class SimultaneousSpeeches(
  statements: Map[Int, DebateSpeech]
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
  distribution: Vector[Double],
  feedback: DebateSpeech 
) extends DebateTurn {
  def timestamp = Some(feedback.timestamp)
}
object JudgeFeedback
object DebateTurn

@Lenses @JsonCodec case class DebateRules(
  fixedOpening: Vector[DebateRoundType],
  repeatingStructure: Vector[DebateRoundType],
  scoringFunction: ScoringFunction
) {
  def turnTypes(numDebaters: Int): LazyList[DebateTurnType] = {
    LazyList.from(fixedOpening.flatMap(_.getTurns(numDebaters))) #:::
      LazyList.continually(repeatingStructure.flatMap(_.getTurns(numDebaters))).flatten
  }
}
object DebateRules {
  def init: DebateRules = DebateRules(
    Vector(
      DebateRoundType.JudgeFeedbackRound(true, 500),
      DebateRoundType.SimultaneousSpeechesRound(500),
      DebateRoundType.JudgeFeedbackRound(true, 500)
    ),
    Vector(
      DebateRoundType.SequentialSpeechesRound(500),
      DebateRoundType.JudgeFeedbackRound(true, 500)
    ),
    ScoringFunction.LogScoreWithLinearPenalty(4, 5, 2, 0.25)
  )
}

@Lenses @JsonCodec case class DebateSetup(
  rules: DebateRules,
  sourceMaterial: Vector[String],
  question: String,
  answers: Vector[String],
  startTime: Long
)
object DebateSetup

@Lenses @JsonCodec case class Debate(
  setup: Option[DebateSetup],
  turns: Vector[DebateTurn]
) {
  // whose turn it is and what they need to submit
  def currentTurn: Option[DebateTurnType] = setup.map { setup =>
    // TODO: validate that the debate follows the specified structure?
    val turnSequence = setup.rules.turnTypes(setup.answers.size)
    if(turns.isEmpty) turnSequence.head else {
      val lastTurnTypeAndRest = turnSequence.drop(turns.size - 1)
      val lastTurn = turns.last
      (lastTurnTypeAndRest.head, lastTurn) match {
        case (DebateTurnType.SimultaneousSpeechesTurn(debaters, charLimit), SimultaneousSpeeches(speeches)) if speeches.size < debaters.size =>
          DebateTurnType.SimultaneousSpeechesTurn(debaters -- speeches.keySet, charLimit)
        case _ => lastTurnTypeAndRest.tail.head
      }
    }
  }
}
object Debate {
  def init: Debate = Debate(None, Vector())
}

@Lenses @JsonCodec case class DebateState(
  debate: Debate,
  participants: Set[ParticipantId]
) {
  def addRole(role: ParticipantId) = {
    copy(participants = participants.filter(_.name != role.name) + role)
  }
}
object DebateState {
  def init = DebateState(Debate.init, Set())
}
