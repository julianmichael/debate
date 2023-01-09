package debate

import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.Prism
import monocle.function.{all => Optics}
import monocle.macros.GenPrism
import monocle.macros.Lenses

import jjm.DotPair
import jjm.implicits._

/** The full data object maintained on the server for each debate. Sent to the
  * client in full each time there's a change to the debate.
  *
  * @param debate
  *   the contents of the debate
  * @param participants
  *   the people currently on the page (w/roles)
  */
@Lenses
@JsonCodec
case class DebateState(debate: Debate, participants: Map[String, Role]) {

  def status: RoomStatus = debate
    .currentTransitions
    .fold(
      _ => RoomStatus.Complete,
      _ =>
        if (debate.rounds.isEmpty)
          RoomStatus.WaitingToBegin
        else
          RoomStatus.InProgress
    )

  /** Add a participant. If the participant is already present, potentially
    * change their role.
    */
  def addParticipant(name: String, role: Role) = copy(participants = participants + (name -> role))

  def canSwitchToRole(userName: String, role: Role) =
    debate.setup.canAssumeRole(userName, role) && !participants.get(userName).exists(_ == role)

}
object DebateState {
  def init(debate: Debate) = DebateState(debate, Map())
}

case class DebateSpeechContent(speakerName: String, timestamp: Long, speech: Vector[SpeechSegment])

case class JudgeFeedbackContent(
  speakerName: String,
  timestamp: Long,
  speech: Vector[SpeechSegment],
  distribution: Vector[Double], // probability distribution
  endDebate: Boolean
)

/** Set of operations available to a particular role. */
case class DebateTransitionSet(
  undo: Map[DebateRole, (Vector[SpeechSegment], Debate)],
  giveSpeech: Map[DebateRole, DotPair[Lambda[A => A => Debate], DebateTurnType]]
) {
  def currentSpeakers = giveSpeech.keySet
  def currentTurns    = giveSpeech.mapVals(_.fst)
}

@JsonCodec
sealed trait DebateEndReason
object DebateEndReason {
  case object JudgeDecided extends DebateEndReason
  case object TimeUp       extends DebateEndReason
}

@Lenses
@JsonCodec
case class JudgingResult(
  correctAnswerIndex: Int,
  numContinues: Int,
  finalJudgement: Vector[Double],
  judgeReward: Double
)
object JudgingResult

@Lenses
@JsonCodec
case class DebateResult(
  correctAnswerIndex: Int,
  endedBy: DebateEndReason,
  judgingInfo: Option[JudgingResult]
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
@Lenses
@JsonCodec
case class Debate(setup: DebateSetup, rounds: Vector[DebateRound]) {

  /** Time of the first round of the debate (not the init time of the debate
    * setup).
    */
  def startTime: Option[Long] = rounds.headOption.flatMap(_.timestamp(setup.numDebaters))

  def result: Option[DebateResult]           = currentTransitions.left.toOption
  def isOver: Boolean                        = result.nonEmpty
  def finalJudgement: Option[Vector[Double]] = result.flatMap(_.judgingInfo.map(_.finalJudgement))

  def numContinues = rounds.foldMap {
    case JudgeFeedback(_, _, false) =>
      1
    case _ =>
      0
  }

  /** Whose turn(s) it is, what they can do, and how to compute the results. */
  def currentTransitions: Either[DebateResult, DebateTransitionSet] = {

    // turn sequence is always nonempty
    val numDebaters   = setup.answers.size
    val roundSequence = setup.rules.roundTypes

    def newRoundSpeeches(roundType: DebateRoundType, isLastTurn: Boolean) = {
      val turn = roundType.getFirstTurn(numDebaters, isLastTurn)
      turn
        .currentRoles
        .map(role =>
          (role: DebateRole) ->
            DotPair[Lambda[I => I => Debate]](turn)(
              turn
                .newRoundTransition(role)
                .andThen(round =>
                  Debate.rounds.modify(_ :+ turn.roundPrism.reverseGet(round))(this)
                )
            )
        )
        .toMap
    }

    // import DebateTurnType._
    def curRoundSpeeches(
      turnType: DebateTurnType
    ): Map[DebateRole, DotPair[Lambda[I => I => Debate], DebateTurnType]] =
      turnType
        .currentRoles
        .map(role =>
          (role: DebateRole) ->
            DotPair[Lambda[I => I => Debate]](turnType)((input: turnType.Input) =>
              Debate
                .rounds
                .composeOptional(Optics.lastOption)
                .composePrism(turnType.roundPrism)
                .modify(turnType.curRoundSpeeches(role)(input))(this)
            )
        )
        .toMap

    // round is done, so we can create a new round with the possibility of undo
    def lastRoundUndos = rounds
      .lastOption
      .map {
        case JudgeFeedback(_, feedback, _) =>
          Map((Judge: DebateRole) -> (feedback.content -> Debate.rounds.modify(_.init)(this)))
        case SimultaneousSpeeches(speeches) =>
          val isOnlySpeech = speeches.size == 1
          speeches.map { case (speakerIndex, speech) =>
            val newDebate =
              if (isOnlySpeech) {
                Debate.rounds.modify(_.init)(this)
              } else
                Debate
                  .rounds
                  .composeOptional(Optics.lastOption)
                  .set(SimultaneousSpeeches(speeches - speakerIndex))(this)

            (Debater(speakerIndex): DebateRole) -> (speech.content -> newDebate)
          }
        case SequentialSpeeches(speeches) =>
          val isOnlySpeech = speeches.size == 1
          speeches.map { case (speakerIndex, speech) =>
            val newDebate =
              if (isOnlySpeech) {
                Debate.rounds.modify(_.init)(this)
              } else
                Debate
                  .rounds
                  .composeOptional(Optics.lastOption)
                  .set(SequentialSpeeches(speeches - speakerIndex))(this)

            (Debater(speakerIndex): DebateRole) -> (speech.content -> newDebate)
          }
      }
      .getOrElse(Map())

    // TODO: validate that the debate follows the specified structure?
    if (rounds.isEmpty)
      Right(
        DebateTransitionSet(
          Map(),
          newRoundSpeeches(roundSequence.head, isLastTurn = roundSequence.tail.isEmpty)
        )
      )
    else {
      // should always be nonempty since the last round should have a round type
      val lastRoundType #:: futureRoundTypes = roundSequence.drop(rounds.size - 1)
      val lastRound                          = rounds.last
      lastRoundType.getTurn(lastRound, numDebaters) match {
        case DebateTurnTypeResult.Next =>
          futureRoundTypes match {
            case LazyList() => // time's up
              Left(
                DebateResult(
                  correctAnswerIndex = setup.correctAnswerIndex,
                  endedBy = DebateEndReason.TimeUp,
                  judgingInfo =
                    lastRound match {
                      case JudgeFeedback(finalJudgement, feedback, endDebate) =>
                        // judge shouldn't be allowed to say 'continue the debate' in the last turn
                        require(endDebate)
                        val judgeReward = setup
                          .rules
                          .scoringFunction
                          .eval(numContinues, finalJudgement, setup.correctAnswerIndex)
                        Some(
                          JudgingResult(
                            correctAnswerIndex = setup.correctAnswerIndex,
                            numContinues = numContinues,
                            finalJudgement = finalJudgement,
                            judgeReward = judgeReward
                          )
                        )
                      case _ =>
                        None
                    }
                )
              )
            case nextRoundType #:: followingRoundTypes =>
              Right(
                DebateTransitionSet(
                  lastRoundUndos,
                  newRoundSpeeches(nextRoundType, isLastTurn = followingRoundTypes.isEmpty)
                )
              )
          }
        case DebateTurnTypeResult.Turn(turn) =>
          Right(DebateTransitionSet(lastRoundUndos, curRoundSpeeches(turn)))
        case DebateTurnTypeResult.End(finalJudgement) =>
          val judgeReward = setup
            .rules
            .scoringFunction
            .eval(numContinues, finalJudgement, setup.correctAnswerIndex)
          Left(
            DebateResult(
              correctAnswerIndex = setup.correctAnswerIndex,
              endedBy = DebateEndReason.JudgeDecided,
              judgingInfo = Some(
                JudgingResult(
                  correctAnswerIndex = setup.correctAnswerIndex,
                  numContinues = numContinues,
                  finalJudgement = finalJudgement,
                  judgeReward = judgeReward
                )
              )
            )
          )
        case DebateTurnTypeResult.Mismatch =>
          ??? // TODO fail gracefully
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
@JsonCodec
sealed trait DebateRound {
  def allSpeeches: Set[DebateSpeech]

  def isComplete(numDebaters: Int): Boolean
  final def timestamp(numDebaters: Int): Option[Long] =
    if (!isComplete(numDebaters))
      None
    else {
      allSpeeches.view.map(_.timestamp).maxOption
    }
}
@Lenses
@JsonCodec
case class SimultaneousSpeeches(
  speeches: Map[Int, DebateSpeech] // map from answer index -> statement
) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches                  = speeches.values.toSet

}
object SimultaneousSpeeches
@Lenses
@JsonCodec
case class SequentialSpeeches(speeches: Map[Int, DebateSpeech]) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches                  = speeches.values.toSet

}
object SequentialSpeeches
@Lenses
@JsonCodec
case class JudgeFeedback(
  distribution: Vector[Double], // probability distribution
  feedback: DebateSpeech,
  endDebate: Boolean
) extends DebateRound {
  def isComplete(numDebaters: Int) = true
  def allSpeeches                  = Set(feedback)
}
object JudgeFeedback
object DebateRound {
  val simultaneousSpeeches = GenPrism[DebateRound, SimultaneousSpeeches]
  val sequentialSpeeches   = GenPrism[DebateRound, SequentialSpeeches]
  val judgeFeedback        = GenPrism[DebateRound, JudgeFeedback]
}

/** Specifies who gets to speak next and what kind of input they should provide.
  */
sealed trait DebateTurnType {
  type AllowedRole <: DebateRole
  type Round
  type Input // type of input the turn expects from a participant of one of the admissible roles
  type Out = Input // for jjm.Dot stuff
  def charLimit: Int
  def quoteLimit: Option[Int]
  def currentRoles: Set[AllowedRole]

  def roundPrism: Prism[DebateRound, Round]
  def newRoundTransition(role: AllowedRole): Input => Round
  def curRoundSpeeches(role: AllowedRole): Input => Round => Round
}
object DebateTurnType {
  case class SimultaneousSpeechesTurn(
    remainingDebaters: Set[Int],
    charLimit: Int,
    quoteLimit: Option[Int]
  ) extends DebateTurnType {
    type AllowedRole = Debater
    type Round       = SimultaneousSpeeches
    type Input       = DebateSpeechContent
    def currentRoles = remainingDebaters.map(Debater(_))

    def roundPrism = DebateRound.simultaneousSpeeches

    def newRoundTransition(role: AllowedRole) = {
      case DebateSpeechContent(name, timestamp, contents) =>
        SimultaneousSpeeches(
          Map(role.answerIndex -> DebateSpeech(ParticipantId(name, role), timestamp, contents))
        )
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeechContent => Round => Round = {
      case DebateSpeechContent(name, timestamp, contents) =>
        val speech = DebateSpeech(ParticipantId(name, role), timestamp, contents)
        SimultaneousSpeeches.speeches.modify(_ + (role.answerIndex -> speech))
    }
  }
  case class DebaterSpeechTurn(debater: Int, charLimit: Int, quoteLimit: Option[Int])
      extends DebateTurnType {
    type AllowedRole = Debater
    type Round       = SequentialSpeeches
    type Input       = DebateSpeechContent
    def currentRoles = Set(Debater(debater))

    def roundPrism = DebateRound.sequentialSpeeches

    def newRoundTransition(role: AllowedRole) = {
      case DebateSpeechContent(name, timestamp, contents) =>
        SequentialSpeeches(
          Map(role.answerIndex -> DebateSpeech(ParticipantId(name, role), timestamp, contents))
        )
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeechContent => Round => Round = {
      case DebateSpeechContent(name, timestamp, contents) =>
        val speech = DebateSpeech(ParticipantId(name, Debater(debater)), timestamp, contents)
        SequentialSpeeches.speeches.modify(_ + (debater -> speech))
    }
  }
  case class JudgeFeedbackTurn(reportBeliefs: Boolean, charLimit: Int, mustEndDebate: Boolean)
      extends DebateTurnType {
    type AllowedRole = Judge.type
    type Round       = JudgeFeedback
    type Input       = JudgeFeedbackContent
    def currentRoles = Set(Judge)
    def quoteLimit   = None
    def roundPrism   = DebateRound.judgeFeedback

    def newRoundTransition(role: AllowedRole) = {
      case JudgeFeedbackContent(name, timestamp, contents, distribution, endDebate) =>
        require(!mustEndDebate || endDebate, "Judge must end the debate.")
        JudgeFeedback(
          distribution = distribution,
          DebateSpeech(ParticipantId(name, role), timestamp, contents),
          endDebate = endDebate
        )
    }

    def curRoundSpeeches(
      role: AllowedRole
    ): JudgeFeedbackContent => JudgeFeedback => JudgeFeedback =
      // no more speeches this round (shouldn't get here)
      ???
  }
}
