package debate

import cats.implicits._

import io.circe.Decoder
import monocle.function.{all => Optics}
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
case class Debate(
  setup: DebateSetup,
  rounds: Vector[DebateRound],
  feedback: Map[String, Feedback.SurveyResponse],
  scratchpads: Map[DebateRole, Vector[Vector[SpeechSegment]]] = Map()
  // TODO premoves
) {
  import Debate.DebateTransitionSet

  def status: RoomStatus = result
    .map(result => RoomStatus.Complete(result, offlineJudgingResults, feedback.keySet))
    .getOrElse(
      if (rounds.isEmpty)
        RoomStatus.WaitingToBegin
      else
        RoomStatus.InProgress
    )

  /** Time of the first round of the debate (not the init time of the debate
    * setup).
    */
  def startTime: Option[Long] = rounds.headOption.flatMap(_.maxTimestamp)

  def isOver: Boolean                        = result.nonEmpty
  def finalJudgement: Option[Vector[Double]] = result.flatMap(_.judgingInfo.map(_.finalJudgement))

  def numDebaters = setup.answers.size

  def numContinues = rounds.foldMap {
    case JudgeFeedback(_, _, false) =>
      1
    case _ =>
      0
  }

  def numDebateRounds = rounds.foldMap {
    case SimultaneousSpeeches(_) | SequentialSpeeches(_) =>
      1
    case _ =>
      0
  }

  def result: Option[DebateResult]                        = stateInfo._1
  def currentTransitions: DebateTransitionSet             = stateInfo._2
  def offlineJudgingResults: Map[String, OfflineJudgment] = stateInfo._3

  /** Whose turn(s) it is, what they can do, and how to compute the results. */
  def stateInfo: (Option[DebateResult], DebateTransitionSet, Map[String, OfflineJudgment]) = {

    val assignedDebaters = setup
      .roles
      .keySet
      .collect { case Debater(i) =>
        i
      }

    // turn sequence is always nonempty
    val roundSequence = setup.rules.roundTypes

    def newRoundSpeeches(roundType: DebateRoundType, isLastTurn: Boolean) = {
      val turn = roundType.getFirstTurn(numDebaters, assignedDebaters, isLastTurn)
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
                    roundTypes = nextRoundTypes,
                    previousRoundOpt = previousRoundOpt
                  )
                )
            case nextRound =>
              nextRoundTypes match {
                case LazyList() =>
                  Left("Too many rounds! Can't match to the debate structure")
                case nextRoundType #:: futureRoundTypes =>
                  nextRoundType.getTurn(nextRound, numDebaters, assignedDebaters) match {
                    case DebateTurnTypeResult.Next =>
                      futureRoundTypes match {
                        case LazyList() => // time's up
                          Right(
                            RoundAcc(
                              result = Some(
                                DebateResult(
                                  timestamp = previousRoundOpt
                                    .flatMap(_.maxTimestamp)
                                    .getOrElse(setup.creationTime),
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
                                timestamp = nextRound
                                  .maxTimestamp
                                  .getOrElse(
                                    previousRoundOpt
                                      .flatMap(_.maxTimestamp)
                                      .getOrElse(setup.creationTime)
                                  ),
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
                                timestamp = nextRound
                                  .maxTimestamp
                                  .getOrElse(
                                    previousRoundOpt
                                      .flatMap(_.maxTimestamp)
                                      .getOrElse(setup.creationTime)
                                  ),
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

    val fillOutOfflineSpeakers = Optics
      .each[Vector[DebateRound], DebateRound]
      .composePrism(DebateRound.offlineJudgments)
      .composeLens(OfflineJudgments.judgments)
      .modify {
        _.transform { case (judge, judgment) =>
          OfflineJudgment
            .judgments
            .composeTraversal(Optics.each)
            .composeLens(JudgeFeedback.feedback)
            .composeLens(DebateSpeech.speaker)
            .set(judge)(judgment)
        }
      }

    val fillInOfflineJudgeAssignments =
      (debate: Debate) =>
        Debate
          .setup
          .composeLens(DebateSetup.offlineJudges)
          .modify { assignedJudges =>
            val newJudgeAssignments = debate
              .offlineJudgingResults
              .filterNot { case (judge, _) =>
                assignedJudges.contains(judge)
              }
              .map { case (judge, res) =>
                judge -> Some(res.mode)
              }
            assignedJudges ++ newJudgeAssignments
          }(debate)

    fillInOfflineJudgeAssignments(this.copy(rounds = fillOutOfflineSpeakers(clampProbs(rounds))))
  }

  def visibleRounds(userName: String, role: Role) = {
    val judgmentsForEachRound = {
      val allJudgmentGroups = this
        .offlineJudgingResults
        .values
        .toVector
        .filter(_.mode == OfflineJudgingMode.Stepped)
        .map(_.judgments)
      val maxNumJudgments = allJudgmentGroups.map(_.size).maximumOption.getOrElse(0)
      (0 until maxNumJudgments)
        .map { n =>
          allJudgmentGroups.flatMap(_.lift(n)).toSet
        }
        .toVector
    }
    this
      .rounds
      .zip(
        this.setup.rules.roundTypes #::: LazyList.continually(DebateRoundType.OfflineJudgingRound)
      )
      .zip(
        this
          .rounds
          .scanLeft(0 -> judgmentsForEachRound.lift(0).combineAll) { case ((n, _), round) =>
            round match {
              case SimultaneousSpeeches(_) | SequentialSpeeches(_) =>
                (n + 1) -> judgmentsForEachRound.lift(n + 1).combineAll
              case _ =>
                n -> Set()
              // case JudgeFeedback(_, _, false) =>
              //   n + 1
              // case _ =>
              //   n
            }
          }
      )
      .flatMap { case ((round, roundType), (numPrevDebateRounds, judgmentsForRound)) =>
        val debaters =
          if (roundType.assignedDebatersOnly)
            this
              .setup
              .roles
              .keySet
              .collect { case Debater(i) =>
                i
              }
          else
            (0 until this.setup.numAnswers).toSet
        val visibleSpeeches = round.visibleSpeechesForRole(role, debaters)
        // number of feedback rounds the current user has given so far as offline judge.
        // The user can only see this round if they have given feedback on all previous rounds where the live judge did.
        val continuationLimitOpt =
          this.offlineJudgingResults.get(userName) match {
            case Some(result) if result.result.isEmpty =>
              Some(result.judgments.size)
            case None if role == OfflineJudge =>
              Some(0)
            case _ =>
              None
          }

        Option(
          Debate
            .VisibleRound(round, roundType, numPrevDebateRounds, judgmentsForRound, visibleSpeeches)
        ).filter(_ =>
          continuationLimitOpt.forall(_ > numPrevDebateRounds) &&
            (role.canSeeIntermediateArguments || round.isComplete(debaters))
        )
      }
  }
}
object Debate {

  implicit val debateEncoder = {
    import io.circe.generic.semiauto._
    deriveEncoder[Debate]
  }

  implicit val debateDecoder =
    // import io.circe.generic.semiauto._
    // deriveDecoder[Debate]
    Decoder.instance { c =>
      for {
        setup    <- c.downField("setup").as[DebateSetup]
        rounds   <- c.downField("rounds").as[Vector[DebateRound]]
        feedback <- c.downField("feedback").as[Map[String, Feedback.SurveyResponse]]
        scratchpads =
          c.downField("scratchpads").as[Map[DebateRole, Vector[Vector[SpeechSegment]]]] match {
            case Right(scratchpads) =>
              scratchpads
            case Left(err) =>
              Map.empty[DebateRole, Vector[Vector[SpeechSegment]]]
          }
      } yield Debate(setup, rounds, feedback, scratchpads)
    }

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

  case class VisibleRound(
    round: DebateRound,
    roundType: DebateRoundType,
    numPreviousDebateRounds: Int,
    offlineJudgmentsForRound: Set[JudgeFeedback],
    visibleSpeeches: Map[Role, DebateSpeech]
  )

}
