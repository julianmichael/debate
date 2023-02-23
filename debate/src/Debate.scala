package debate

import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.Prism
import monocle.function.{all => Optics}
import monocle.macros.GenPrism
import monocle.macros.Lenses

import jjm.DotPair
import jjm.implicits._

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
case class Debate(
  setup: DebateSetup,
  rounds: Vector[DebateRound],
  offlineJudgingResults: Map[String, OfflineJudgingInfo],
  feedback: Map[String, Feedback.SurveyResponse]
) {
  import Debate.DebateTransitionSet

  /** Time of the first round of the debate (not the init time of the debate
    * setup).
    */
  def startTime: Option[Long] = rounds.headOption.flatMap(_.timestamp(setup.numDebaters))

  def isOver: Boolean                        = result.nonEmpty
  def finalJudgement: Option[Vector[Double]] = result.flatMap(_.judgingInfo.map(_.finalJudgement))

  def numContinues = rounds.foldMap {
    case JudgeFeedback(_, _, false) =>
      1
    case _ =>
      0
  }

  def result: Option[DebateResult]            = stateInfo._1
  def currentTransitions: DebateTransitionSet = stateInfo._2

  // TODO: go turn-by-turn to make sure we capture when the debate ends.
  // this should really be a fold over the rounds.
  /** Whose turn(s) it is, what they can do, and how to compute the results. */
  def stateInfo: (Option[DebateResult], DebateTransitionSet) = {

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

    def lastRoundUndos: Map[DebateRole, (Vector[SpeechSegment], Debate)] = rounds
      .lastOption
      .map {
        // TODO XXX: prevent judge from undoing if debate is done
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
        case NegotiateEnd(votes) =>
          val isOnlyVote = votes.size == 1
          votes.map { case (voterIndex, vote) =>
            val newDebate =
              if (isOnlyVote) {
                Debate.rounds.modify(_.init)(this)
              } else
                Debate
                  .rounds
                  .composeOptional(Optics.lastOption)
                  .set(NegotiateEnd(votes - voterIndex))(this)

            (Debater(voterIndex): DebateRole) -> (Vector() -> newDebate)
          }
        case OfflineJudgment(_, _, _, _) =>
          Map.empty[DebateRole, (Vector[SpeechSegment], Debate)]
      }
      .getOrElse(Map.empty[DebateRole, (Vector[SpeechSegment], Debate)])

    // TODO: validate that the debate follows the specified structure?
    if (rounds.isEmpty)
      None ->
        DebateTransitionSet(
          Map(),
          newRoundSpeeches(roundSequence.head, isLastTurn = roundSequence.tail.isEmpty)
        )
    else {
      // should always be nonempty since the last round should have a round type
      val lastRoundType #:: futureRoundTypes = roundSequence.drop(rounds.size - 1)
      val lastRound                          = rounds.last
      lastRoundType.getTurn(lastRound, numDebaters) match {
        case DebateTurnTypeResult.Next =>
          futureRoundTypes match {
            case LazyList() => // time's up
              Some(
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
                // TODO XXX offline judging turn type
              ) ->
                DebateTransitionSet(
                  undo = Map(),
                  giveSpeech = newRoundSpeeches(DebateRoundType.OfflineJudgingRound, false)
                )
            case nextRoundType #:: followingRoundTypes =>
              None ->
                DebateTransitionSet(
                  lastRoundUndos,
                  newRoundSpeeches(nextRoundType, isLastTurn = followingRoundTypes.isEmpty)
                )
          }
        case DebateTurnTypeResult.Turn(turn) =>
          None -> DebateTransitionSet(lastRoundUndos, curRoundSpeeches(turn))
        case DebateTurnTypeResult.EndByJudge(finalJudgement) =>
          // TODO: change the End argument to itself contain all/most of the info we need
          // so it is constructed where more appropriate (ie in the turn transition)
          val judgeReward = setup
            .rules
            .scoringFunction
            .eval(numContinues, finalJudgement, setup.correctAnswerIndex)
          Some(
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
          ) ->
            DebateTransitionSet(
              undo = Map(),
              giveSpeech = newRoundSpeeches(DebateRoundType.OfflineJudgingRound, false)
            )
        case DebateTurnTypeResult.EndByAgreement =>
          Some(
            DebateResult(
              correctAnswerIndex = setup.correctAnswerIndex,
              endedBy = DebateEndReason.MutualAgreement,
              judgingInfo = None
            )
          ) ->
            DebateTransitionSet(
              undo = Map(),
              giveSpeech = newRoundSpeeches(DebateRoundType.OfflineJudgingRound, false)
            )
        case DebateTurnTypeResult.Mismatch =>
          System.err.println(lastRoundType)
          System.err.println(lastRound)
          ??? // TODO fail gracefully
      }
    }
  }

  def clean = this.copy(rounds =
    rounds.map {
      case JudgeFeedback(dist, feedback, endDebate) =>
        val newDist =
          dist
            .zipWithIndex
            .foldLeft(dist) { case (d, (prob, idx)) =>
              if (prob < 0.01)
                Utils.adjustProbability(d, idx, 0.01)
              else if (prob > 0.99)
                Utils.adjustProbability(d, idx, 0.99)
              else
                d
            }
        JudgeFeedback(newDist, feedback, endDebate)
      case x =>
        x
    }
  )
}
object Debate {

  /** Set of operations available to a particular role. */
  case class DebateTransitionSet(
    undo: Map[DebateRole, (Vector[SpeechSegment], Debate)],
    giveSpeech: Map[DebateRole, DotPair[Lambda[A => A => Debate], DebateTurnType]]
  ) {
    def currentSpeakers = giveSpeech.keySet
    def currentTurns    = giveSpeech.mapVals(_.fst)
  }

  def init(setup: DebateSetup): Debate = Debate(setup, Vector(), Map(), Map())
}

/** Outcome of a debate turn after some/all relevant parties have submitted
  * their arguments / info
  */
@JsonCodec
sealed trait DebateRound {
  def allSpeeches: Map[Role, DebateSpeech]

  def isComplete(numDebaters: Int): Boolean
  final def timestamp(numDebaters: Int): Option[Long] =
    if (!isComplete(numDebaters))
      None
    else {
      allSpeeches.values.view.map(_.timestamp).maxOption
    }
}

@Lenses
@JsonCodec
case class SimultaneousSpeeches(
  speeches: Map[Int, DebateSpeech] // map from answer index -> statement
) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches = speeches.map { case (idx, speech) =>
    Debater(idx) -> speech
  }

}
object SimultaneousSpeeches

@Lenses
@JsonCodec
case class SequentialSpeeches(speeches: Map[Int, DebateSpeech]) extends DebateRound {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches = speeches.map { case (idx, speech) =>
    Debater(idx) -> speech
  }

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
  def allSpeeches                  = Map(Judge -> feedback)
}
object JudgeFeedback

@Lenses
@JsonCodec
case class NegotiateEnd(votes: Map[Int, Boolean]) extends DebateRound {
  def isComplete(numDebaters: Int) = votes.size == numDebaters
  def allSpeeches                  = Map()
}
object NegotiateEnd

@Lenses
@JsonCodec
case class OfflineJudgment(
  judge: String,
  startTimeMillis: Long,
  numContinues: Int,
  result: Option[OfflineJudgingResult]
) extends DebateRound {
  def isComplete(numDebaters: Int) = result.nonEmpty
  def allSpeeches = result
    .map { case OfflineJudgingResult(_, explanation, timestamp) =>
      Map(
        (TimedOfflineJudge: Role) ->
          DebateSpeech(judge, timestamp, Vector(SpeechSegment.Text(explanation)))
      )
    }
    .getOrElse(Map.empty[Role, DebateSpeech])
}
object OfflineJudgment

object DebateRound {
  val simultaneousSpeeches = GenPrism[DebateRound, SimultaneousSpeeches]
  val sequentialSpeeches   = GenPrism[DebateRound, SequentialSpeeches]
  val judgeFeedback        = GenPrism[DebateRound, JudgeFeedback]
  val negotiateEnd         = GenPrism[DebateRound, NegotiateEnd]
  val offlineJudgment      = GenPrism[DebateRound, OfflineJudgment]
}

/** Specifies who gets to speak next and what kind of input they should provide.
  */
sealed trait DebateTurnType {
  type AllowedRole <: DebateRole
  type Round
  type Input // type of input the turn expects from a participant of one of the admissible roles
  type Out = Input // for jjm.Dot stuff
  def charLimitOpt: Option[Int]
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
    type Input       = DebateSpeech
    def currentRoles = remainingDebaters.map(Debater(_))
    def charLimitOpt = Some(charLimit)
    def roundPrism   = DebateRound.simultaneousSpeeches

    def newRoundTransition(role: AllowedRole) = { case DebateSpeech(name, timestamp, contents) =>
      SimultaneousSpeeches(Map(role.answerIndex -> DebateSpeech(name, timestamp, contents)))
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeech => Round => Round = {
      case DebateSpeech(name, timestamp, contents) =>
        val speech = DebateSpeech(name, timestamp, contents)
        SimultaneousSpeeches.speeches.modify(_ + (role.answerIndex -> speech))
    }
  }
  case class DebaterSpeechTurn(debater: Int, charLimit: Int, quoteLimit: Option[Int])
      extends DebateTurnType {
    type AllowedRole = Debater
    type Round       = SequentialSpeeches
    type Input       = DebateSpeech
    def currentRoles = Set(Debater(debater))
    def charLimitOpt = Some(charLimit)
    def roundPrism   = DebateRound.sequentialSpeeches

    def newRoundTransition(role: AllowedRole) = { case DebateSpeech(name, timestamp, contents) =>
      SequentialSpeeches(Map(role.answerIndex -> DebateSpeech(name, timestamp, contents)))
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeech => Round => Round = {
      case DebateSpeech(name, timestamp, contents) =>
        val speech = DebateSpeech(name, timestamp, contents)
        SequentialSpeeches.speeches.modify(_ + (debater -> speech))
    }
  }
  case class JudgeFeedbackTurn(reportBeliefs: Boolean, charLimit: Int, mustEndDebate: Boolean)
      extends DebateTurnType {
    type AllowedRole = Judge.type
    type Round       = JudgeFeedback
    type Input       = JudgeFeedback
    def charLimitOpt = Some(charLimit)
    def currentRoles = Set(Judge)
    def quoteLimit   = None
    def roundPrism   = DebateRound.judgeFeedback

    def newRoundTransition(role: AllowedRole) = { judgeFeedback =>
      require(!mustEndDebate || judgeFeedback.endDebate, "Judge must end the debate.")
      judgeFeedback
    }

    def curRoundSpeeches(role: AllowedRole): JudgeFeedback => JudgeFeedback => JudgeFeedback =
      // no more speeches this round (shouldn't get here)
      ???
  }

  case class NegotiateEndTurn(remainingDebaters: Set[Int]) extends DebateTurnType {
    type AllowedRole = Debater
    type Round       = NegotiateEnd
    type Input       = Boolean

    def charLimitOpt = None
    def quoteLimit   = None

    def currentRoles = remainingDebaters.map(Debater(_))

    def roundPrism = DebateRound.negotiateEnd

    def newRoundTransition(role: AllowedRole) = { case votesForEnd =>
      NegotiateEnd(Map(role.answerIndex -> votesForEnd))
    }

    def curRoundSpeeches(role: AllowedRole): Boolean => Round => Round = { case votesForEnd =>
      NegotiateEnd.votes.modify(_ + (role.answerIndex -> votesForEnd))
    }
  }

  case object OfflineJudgingTurn extends DebateTurnType {
    type AllowedRole = TimedOfflineJudge.type
    type Round       = OfflineJudgment
    type Input       = OfflineJudgment
    def charLimitOpt = None
    def currentRoles = Set(TimedOfflineJudge)
    def quoteLimit   = None
    def roundPrism   = DebateRound.offlineJudgment

    def newRoundTransition(role: AllowedRole) = identity[OfflineJudgment]

    def curRoundSpeeches(role: AllowedRole): OfflineJudgment => OfflineJudgment => OfflineJudgment =
      input => _ => input // replace the round with the current input
  }
}
