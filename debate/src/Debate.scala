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
  // TODO rename this back to normal
  def offlineJudgingResults = currentTransitions
    .giveSpeech
    .get(OfflineJudge)
    .map(_.fst)
    .collect { case DebateTurnType.OfflineJudgingTurn(judgments) =>
      judgments
    }
    .getOrElse(Map[String, OfflineJudgment]())

  /** Whose turn(s) it is, what they can do, and how to compute the results. */
  def stateInfo: (Option[DebateResult], DebateTransitionSet, Map[String, OfflineJudgment]) = {

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
        case OfflineJudgments(_) =>
          Map.empty[DebateRole, (Vector[SpeechSegment], Debate)]
      }
      .getOrElse(Map.empty[DebateRole, (Vector[SpeechSegment], Debate)])

    val startDebate = DebateTransitionSet(giveSpeech =
      newRoundSpeeches(roundSequence.head, isLastTurn = roundSequence.tail.isEmpty)
    )

    val startOfflineJudging = DebateTransitionSet(
      undo = Map(),
      giveSpeech = newRoundSpeeches(DebateRoundType.OfflineJudgingRound, false)
    )

    case class RoundAcc(
      result: Option[DebateResult],
      transitions: DebateTransitionSet,
      offlineJudgments: Option[Map[String, OfflineJudgment]],
      roundTypes: LazyList[DebateRoundType],
      previousRoundOpt: Option[DebateRound]
    )
    val roundAccEither =
      rounds.foldM[Either[String, *], RoundAcc](
        RoundAcc(None, startDebate, None, roundSequence, None)
      ) {
        case (
              RoundAcc(resultOpt, _, offlineJudgments, nextRoundTypes, previousRoundOpt),
              nextRound
            ) =>
          nextRound match {
            case OfflineJudgments(judgments) =>
              if (resultOpt.isEmpty)
                Left("Offline judging can only happen after the debate is finished")
              else if (offlineJudgments.nonEmpty)
                Left("Offline judging can only happen once")
              else
                Right(
                  RoundAcc(
                    result = resultOpt,
                    transitions = DebateTransitionSet(giveSpeech =
                      curRoundSpeeches(DebateTurnType.OfflineJudgingTurn(judgments))
                    ),
                    offlineJudgments = Some(judgments),
                    roundTypes = nextRoundTypes, // TODO maybe empty roundTypes?
                    previousRoundOpt = previousRoundOpt
                  )
                )
            case nextRound =>
              nextRoundTypes match {
                case LazyList() =>
                  Left("Too many rounds! Can't match to the debate structure")
                case nextRoundType #:: futureRoundTypes =>
                  nextRoundType.getTurn(nextRound, numDebaters) match {
                    case DebateTurnTypeResult.Next =>
                      futureRoundTypes match {
                        case LazyList() => // time's up
                          Right(
                            RoundAcc(
                              result = Some(
                                DebateResult(
                                  correctAnswerIndex = setup.correctAnswerIndex,
                                  endedBy = DebateEndReason.TimeUp,
                                  judgingInfo =
                                    previousRoundOpt match {
                                      case Some(
                                            JudgeFeedback(finalJudgement, feedback, endDebate)
                                          ) =>
                                        // judge shouldn't be allowed to say 'continue the debate' in the last turn
                                        require(endDebate)
                                        val judgeReward = setup
                                          .rules
                                          .scoringFunction
                                          .eval(
                                            numContinues,
                                            finalJudgement,
                                            setup.correctAnswerIndex
                                          )
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
                              ),
                              transitions = startOfflineJudging,
                              roundTypes = futureRoundTypes,
                              offlineJudgments = offlineJudgments,
                              previousRoundOpt = Some(nextRound)
                            )
                          )
                        case nextRoundType #:: followingRoundTypes =>
                          Right(
                            RoundAcc(
                              result = resultOpt,
                              transitions = DebateTransitionSet(
                                lastRoundUndos,
                                newRoundSpeeches(
                                  nextRoundType,
                                  isLastTurn = followingRoundTypes.isEmpty
                                )
                              ),
                              roundTypes = futureRoundTypes,
                              offlineJudgments = offlineJudgments,
                              previousRoundOpt = Some(nextRound)
                            )
                          )
                      }
                    case DebateTurnTypeResult.Turn(turn) =>
                      Right(
                        RoundAcc(
                          result = resultOpt,
                          transitions = DebateTransitionSet(lastRoundUndos, curRoundSpeeches(turn)),
                          roundTypes = futureRoundTypes,
                          offlineJudgments = offlineJudgments,
                          previousRoundOpt = Some(nextRound)
                        )
                      )
                    case DebateTurnTypeResult.EndByJudge(finalJudgement) =>
                      if (resultOpt.nonEmpty) {
                        Left("Tried to end debate by judge after it already ended")
                      } else {
                        val judgeReward = setup
                          .rules
                          .scoringFunction
                          .eval(numContinues, finalJudgement, setup.correctAnswerIndex)
                        Right(
                          RoundAcc(
                            result = Some(
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
                            ),
                            transitions = startOfflineJudging,
                            roundTypes = futureRoundTypes,
                            offlineJudgments = offlineJudgments,
                            previousRoundOpt = Some(nextRound)
                          )
                        )
                      }
                    case DebateTurnTypeResult.EndByAgreement =>
                      if (resultOpt.nonEmpty) {
                        Left("Tried to end debate by judge after it already ended")
                      } else {
                        Right(
                          RoundAcc(
                            result = Some(
                              DebateResult(
                                correctAnswerIndex = setup.correctAnswerIndex,
                                endedBy = DebateEndReason.MutualAgreement,
                                judgingInfo = None
                              )
                            ),
                            transitions = startOfflineJudging,
                            roundTypes = futureRoundTypes,
                            offlineJudgments = offlineJudgments,
                            previousRoundOpt = Some(nextRound)
                          )
                        )
                      }
                    case DebateTurnTypeResult.Mismatch =>
                      System.err.println(nextRound)
                      System.err.println(nextRoundType)
                      Left(s"Mismatched round and round type!\n$nextRoundType\n$nextRound")
                  }
              }
          }
      }
    roundAccEither match {
      case Left(err) =>
        System.err.println("Error processing debate structure:")
        System.err.println(err)
        ??? // can do something more graceful later; this should never happen
      case Right(acc) =>
        (
          acc.result,
          acc.transitions,
          acc.offlineJudgments.getOrElse(Map.empty[String, OfflineJudgment])
        )
    }
  }

  def clean = {
    def clampProbs(rounds: Vector[DebateRound]) = rounds.map {
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

    this.copy(rounds = clampProbs(rounds))
  }
}
object Debate {

  /** Set of operations available to a particular role. */
  case class DebateTransitionSet(
    undo: Map[DebateRole, (Vector[SpeechSegment], Debate)] =
      Map[DebateRole, (Vector[SpeechSegment], Debate)](),
    giveSpeech: Map[DebateRole, DotPair[Lambda[A => A => Debate], DebateTurnType]] =
      Map[DebateRole, DotPair[Lambda[A => A => Debate], DebateTurnType]]()
  ) {
    def currentSpeakers = giveSpeech.keySet
    def currentTurns    = giveSpeech.mapVals(_.fst)
  }

  def init(setup: DebateSetup): Debate = Debate(setup, Vector(), Map())
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
case class OfflineJudgments(judgments: Map[String, OfflineJudgment]) extends DebateRound {
  def isComplete(numDebaters: Int) = judgments.values.forall(_.result.nonEmpty)
  def allSpeeches =
    judgments
      .toVector
      .view
      .flatMap { case (judge, judgment) =>
        judgment
          .result
          .map { case OfflineJudgingResult(_, explanation, timestamp) =>
            (OfflineJudge: Role) ->
              DebateSpeech(judge, timestamp, Vector(SpeechSegment.Text(explanation)))
          }
      }
      .toMap
}
object OfflineJudgments

object DebateRound {
  val simultaneousSpeeches = GenPrism[DebateRound, SimultaneousSpeeches]
  val sequentialSpeeches   = GenPrism[DebateRound, SequentialSpeeches]
  val judgeFeedback        = GenPrism[DebateRound, JudgeFeedback]
  val negotiateEnd         = GenPrism[DebateRound, NegotiateEnd]
  val offlineJudgments     = GenPrism[DebateRound, OfflineJudgments]
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

  case class OfflineJudgingTurn(existingJudgments: Map[String, OfflineJudgment])
      extends DebateTurnType {
    type AllowedRole = OfflineJudge.type
    type Round       = OfflineJudgments
    type Input       = (String, OfflineJudgment)
    def charLimitOpt = None
    def currentRoles = Set(OfflineJudge)
    def quoteLimit   = None
    def roundPrism   = DebateRound.offlineJudgments

    def newRoundTransition(role: AllowedRole) = judgmentPair => OfflineJudgments(Map(judgmentPair))

    def curRoundSpeeches(
      role: AllowedRole
    ): ((String, OfflineJudgment)) => OfflineJudgments => OfflineJudgments =
      input => OfflineJudgments.judgments.modify(_ + input)
  }
}
