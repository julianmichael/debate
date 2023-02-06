package debate

import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.Prism
import monocle.std.{all => StdOptics}
import monocle.function.{all => Optics}
import monocle.macros.GenPrism
import monocle.macros.Lenses

import jjm.DotPair
import jjm.implicits._
import monocle.Optional

sealed trait DebateTurnTypeResult2 extends Product with Serializable
object DebateTurnTypeResult2 {
  case class Turn(turn: DebateTurnType2)                extends DebateTurnTypeResult2
  case object Next                                      extends DebateTurnTypeResult2
  case class EndByJudge(finalJudgement: Vector[Double]) extends DebateTurnTypeResult2
  case object EndByAgreement                            extends DebateTurnTypeResult2
  case object Mismatch                                  extends DebateTurnTypeResult2
}

/** Schema for round types used to set up the debate. */
@JsonCodec
sealed trait DebateRoundType2 {
  def charLimitOpt: Option[Int]
  def hasJudge: Boolean
  def canEndDebate: Boolean

  import DebateRoundType2._
  def summary(defaultCharLimit: Option[Int]): String = {
    def clStr(charLimit: Int) =
      if (defaultCharLimit.exists(_ == charLimit))
        ""
      else
        charLimit.toString

    this match {
      case SimultaneousSpeechesRound(charLimit, quoteLimit) =>
        val quoteLimitStr = quoteLimit.foldMap(x => s"c$x")
        s"sim${clStr(charLimit)}$quoteLimitStr"
      case SequentialSpeechesRound(charLimit, quoteLimit) =>
        val quoteLimitStr = quoteLimit.foldMap(x => s"c$x")
        s"seq${clStr(charLimit)}$quoteLimitStr"
      case JudgeFeedbackRound(reportBeliefs, charLimit) =>
        val reportBeliefsStr =
          if (reportBeliefs)
            "b"
          else
            ""
        s"j${clStr(charLimit)}$reportBeliefsStr"
      case NegotiateEndRound =>
        "end?"
    }
  }

  def getFirstTurn(numDebaters: Int, isLastTurn: Boolean): DebateTurnType2 = {
    require(numDebaters > 0)
    this match {
      case SimultaneousSpeechesRound(charLimit, quoteLimit) =>
        DebateTurnType2.SimultaneousSpeechesTurn((0 until numDebaters).toSet, charLimit, quoteLimit)
      case SequentialSpeechesRound(charLimit, quoteLimit) =>
        DebateTurnType2.DebaterSpeechTurn(0, charLimit, quoteLimit)
      case JudgeFeedbackRound(reportBeliefs, charLimit) =>
        DebateTurnType2.JudgeFeedbackTurn(reportBeliefs, charLimit, mustEndDebate = isLastTurn)
      case NegotiateEndRound =>
        DebateTurnType2.NegotiateEndTurn((0 until numDebaters).toSet)
    }
  }

  // returns None if round type and round are incompatible
  def getTurn(round: DebateRound2, numDebaters: Int): DebateTurnTypeResult2 =
    (this, round) match {
      case (SimultaneousSpeechesRound(charLimit, quoteLimit), SimultaneousSpeeches2(speeches)) =>
        if (speeches.size == numDebaters)
          DebateTurnTypeResult2.Next
        else {
          DebateTurnTypeResult2.Turn(
            DebateTurnType2.SimultaneousSpeechesTurn(
              (0 until numDebaters).toSet -- speeches.keySet,
              charLimit,
              quoteLimit
            )
          )
        }
      case (SequentialSpeechesRound(charLimit, quoteLimit), SequentialSpeeches2(speeches)) =>
        val remainingDebaters = (0 until numDebaters).toSet -- speeches.keySet
        remainingDebaters
          .minOption
          .fold(DebateTurnTypeResult2.Next: DebateTurnTypeResult2) { nextDebater =>
            DebateTurnTypeResult2
              .Turn(DebateTurnType2.DebaterSpeechTurn(nextDebater, charLimit, quoteLimit))
          }
      case (JudgeFeedbackRound(_, _), JudgeFeedback2(judgment, _, endDebate)) =>
        if (!endDebate)
          DebateTurnTypeResult2.Next
        else
          DebateTurnTypeResult2.EndByJudge(judgment)
      case (NegotiateEndRound, NegotiateEnd2(votes)) =>
        if (votes.size == numDebaters) {
          if (votes.values.forall(identity))
            DebateTurnTypeResult2.EndByAgreement
          else
            DebateTurnTypeResult2.Next
        } else {
          DebateTurnTypeResult2
            .Turn(DebateTurnType2.NegotiateEndTurn((0 until numDebaters).toSet -- votes.keySet))
        }
      case _ =>
        DebateTurnTypeResult2.Mismatch
    }
}
object DebateRoundType2 {
  def fromDebateRoundType(rt: DebateRoundType): DebateRoundType2 =
    rt match {
      case DebateRoundType.SimultaneousSpeechesRound(charLimit, quoteLimit) =>
        SimultaneousSpeechesRound(charLimit, quoteLimit)
      case DebateRoundType.SequentialSpeechesRound(charLimit, quoteLimit) =>
        SequentialSpeechesRound(charLimit, quoteLimit)
      case DebateRoundType.JudgeFeedbackRound(reportBeliefs, charLimit) =>
        JudgeFeedbackRound(reportBeliefs, charLimit)
      case DebateRoundType.NegotiateEndRound =>
        NegotiateEndRound
    }

  @Lenses
  @JsonCodec
  case class SimultaneousSpeechesRound(charLimit: Int, quoteLimit: Option[Int])
      extends DebateRoundType2 {

    def charLimitOpt = Some(charLimit)
    def hasJudge     = false
    def canEndDebate = false
  }
  val simultaneousSpeechesRound = GenPrism[DebateRoundType2, SimultaneousSpeechesRound]

  @Lenses
  @JsonCodec
  case class SequentialSpeechesRound(charLimit: Int, quoteLimit: Option[Int])
      extends DebateRoundType2 {

    def charLimitOpt = Some(charLimit)
    def hasJudge     = false
    def canEndDebate = false
  }
  val sequentialSpeechesRound = GenPrism[DebateRoundType2, SequentialSpeechesRound]

  @Lenses
  @JsonCodec
  case class JudgeFeedbackRound(reportBeliefs: Boolean, charLimit: Int) extends DebateRoundType2 {
    def charLimitOpt = Some(charLimit)
    def hasJudge     = true
    def canEndDebate = true
  }
  val judgeFeedbackRound = GenPrism[DebateRoundType2, JudgeFeedbackRound]

  case object NegotiateEndRound extends DebateRoundType2 {
    def charLimitOpt = None
    def hasJudge     = false
    def canEndDebate = true
  }
  val negotiateEnd = GenPrism[DebateRoundType2, NegotiateEndRound.type]
}

@JsonCodec
case class DebateSpeech2(speaker: String, timestamp: Long, content: Vector[SpeechSegment]) {
  def allQuotes = content.collect { case SpeechSegment.Quote(span) =>
    span
  }
}
object DebateSpeech2 {
  def fromSpeech(speech: DebateSpeech) = DebateSpeech2(
    speech.speaker.name,
    speech.timestamp,
    speech.content
  )
}

/** Set of operations available to a particular role. */
case class DebateTransitionSet2(
  undo: Map[DebateRole, (Vector[SpeechSegment], Debate2)],
  giveSpeech: Map[DebateRole, DotPair[Lambda[A => A => Debate2], DebateTurnType2]]
) {
  def currentSpeakers = giveSpeech.keySet
  def currentTurns    = giveSpeech.mapVals(_.fst)
}

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
case class Debate2(
  setup: DebateSetup,
  rounds: Vector[DebateRound2],
  offlineJudgingResults: Map[String, OfflineJudgingResult],
  feedback: Map[String, Unit] // placeholder for when we add it in later
) {

  /** Time of the first round of the debate (not the init time of the debate
    * setup).
    */
  def startTime: Option[Long] = rounds.headOption.flatMap(_.timestamp(setup.numDebaters))

  def result: Option[DebateResult]           = currentTransitions.left.toOption
  def isOver: Boolean                        = result.nonEmpty
  def finalJudgement: Option[Vector[Double]] = result.flatMap(_.judgingInfo.map(_.finalJudgement))

  def numContinues = rounds.foldMap {
    case JudgeFeedback2(_, _, false) =>
      1
    case _ =>
      0
  }

  /** Whose turn(s) it is, what they can do, and how to compute the results. */
  def currentTransitions: Either[DebateResult, DebateTransitionSet2] = {

    // turn sequence is always nonempty
    val numDebaters   = setup.answers.size
    val roundSequence = setup.rules.roundTypes.map(DebateRoundType2.fromDebateRoundType)

    def newRoundSpeeches(roundType: DebateRoundType2, isLastTurn: Boolean) = {
      val turn = roundType.getFirstTurn(numDebaters, isLastTurn)
      turn
        .currentRoles
        .map(role =>
          (role: DebateRole) ->
            DotPair[Lambda[I => I => Debate2]](turn)(
              turn
                .newRoundTransition(role)
                .andThen(round =>
                  Debate2.rounds.modify(_ :+ turn.roundPrism.reverseGet(round))(this)
                )
            )
        )
        .toMap
    }

    // import DebateTurnType._
    def curRoundSpeeches(
      turnType: DebateTurnType2
    ): Map[DebateRole, DotPair[Lambda[I => I => Debate2], DebateTurnType2]] =
      turnType
        .currentRoles
        .map(role =>
          (role: DebateRole) ->
            DotPair[Lambda[I => I => Debate2]](turnType)((input: turnType.Input) =>
              Debate2
                .rounds
                .composeOptional(Optics.lastOption)
                .composePrism(turnType.roundPrism)
                .modify(turnType.curRoundSpeeches(role)(input))(this)
            )
        )
        .toMap

    // round is done, so we can create a new round with the possibility of undo

    def lastRoundUndos: Map[DebateRole, (Vector[SpeechSegment], Debate2)] = rounds
      .lastOption
      .map {
        case JudgeFeedback2(_, feedback, _) =>
          Map((Judge: DebateRole) -> (feedback.content -> Debate2.rounds.modify(_.init)(this)))
        case SimultaneousSpeeches2(speeches) =>
          val isOnlySpeech = speeches.size == 1
          speeches.map { case (speakerIndex, speech) =>
            val newDebate =
              if (isOnlySpeech) {
                Debate2.rounds.modify(_.init)(this)
              } else
                Debate2
                  .rounds
                  .composeOptional(Optics.lastOption)
                  .set(SimultaneousSpeeches2(speeches - speakerIndex))(this)

            (Debater(speakerIndex): DebateRole) -> (speech.content -> newDebate)
          }
        case SequentialSpeeches2(speeches) =>
          val isOnlySpeech = speeches.size == 1
          speeches.map { case (speakerIndex, speech) =>
            val newDebate =
              if (isOnlySpeech) {
                Debate2.rounds.modify(_.init)(this)
              } else
                Debate2
                  .rounds
                  .composeOptional(Optics.lastOption)
                  .set(SequentialSpeeches2(speeches - speakerIndex))(this)

            (Debater(speakerIndex): DebateRole) -> (speech.content -> newDebate)
          }
        case NegotiateEnd2(votes) =>
          val isOnlyVote = votes.size == 1
          votes.map { case (voterIndex, vote) =>
            val newDebate =
              if (isOnlyVote) {
                Debate2.rounds.modify(_.init)(this)
              } else
                Debate2
                  .rounds
                  .composeOptional(Optics.lastOption)
                  .set(NegotiateEnd2(votes - voterIndex))(this)

            (Debater(voterIndex): DebateRole) -> (Vector() -> newDebate)
          }
      }
      .getOrElse(Map())

    // TODO: validate that the debate follows the specified structure?
    if (rounds.isEmpty)
      Right(
        DebateTransitionSet2(
          Map(),
          newRoundSpeeches(roundSequence.head, isLastTurn = roundSequence.tail.isEmpty)
        )
      )
    else {
      // should always be nonempty since the last round should have a round type
      val lastRoundType #:: futureRoundTypes = roundSequence.drop(rounds.size - 1)
      val lastRound                          = rounds.last
      lastRoundType.getTurn(lastRound, numDebaters) match {
        case DebateTurnTypeResult2.Next =>
          futureRoundTypes match {
            case LazyList() => // time's up
              Left(
                DebateResult(
                  correctAnswerIndex = setup.correctAnswerIndex,
                  endedBy = DebateEndReason.TimeUp,
                  judgingInfo =
                    lastRound match {
                      case JudgeFeedback2(finalJudgement, feedback, endDebate) =>
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
                DebateTransitionSet2(
                  lastRoundUndos,
                  newRoundSpeeches(nextRoundType, isLastTurn = followingRoundTypes.isEmpty)
                )
              )
          }
        case DebateTurnTypeResult2.Turn(turn) =>
          Right(DebateTransitionSet2(lastRoundUndos, curRoundSpeeches(turn)))
        case DebateTurnTypeResult2.EndByJudge(finalJudgement) =>
          // TODO: change the End argument to itself contain all/most of the info we need
          // so it is constructed where more appropriate (ie in the turn transition)
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
        case DebateTurnTypeResult2.EndByAgreement =>
          Left(
            DebateResult(
              correctAnswerIndex = setup.correctAnswerIndex,
              endedBy = DebateEndReason.MutualAgreement,
              judgingInfo = None
            )
          )
        case DebateTurnTypeResult2.Mismatch =>
          ??? // TODO fail gracefully
      }
    }
  }
}
object Debate2 {
  def init(setup: DebateSetup): Debate2 = Debate2(setup, Vector(), Map(), Map())

  def fromDebate(debate: Debate) = {

    val numDebaters  = debate.setup.numDebaters
    val wasJudgeless = debate.rounds.exists(_.allSpeeches.exists(_.speaker.name == "Nobody"))

    def modRounds(whichRounds: Optional[DebateRules, Vector[DebateRoundType]]) = DebateSetup
      .rules
      .composeOptional(whichRounds)
      .composeTraversal(Optics.each)
      .modify {
        case DebateRoundType.JudgeFeedbackRound(reportBeliefs, charLimit) =>
          DebateRoundType.NegotiateEndRound
        case x =>
          x
      }

    val setup =
      modRounds(DebateRules.fixedOpening.asOptional)
        .andThen(modRounds(DebateRules.repeatingStructure.asOptional))
        .andThen(
          modRounds(
            DebateRules
              .fixedClosing
              .composePrism(StdOptics.some[ClosingArgumentRules])
              .composeLens(ClosingArgumentRules.rounds)
          )
        )(debate.setup)

    val rounds = {
      val initRounds = debate
        .rounds
        .map {
          case SimultaneousSpeeches(speeches) =>
            SimultaneousSpeeches2(speeches.mapVals(DebateSpeech2.fromSpeech))
          case JudgeFeedback(dist, feedback, end) =>
            if (feedback.speaker.name == "Nobody") {
              NegotiateEnd2((0 until numDebaters).map(_ -> false).toMap)
            } else {
              JudgeFeedback2(dist, DebateSpeech2.fromSpeech(feedback), end)
            }
          case NegotiateEnd(votes) =>
            NegotiateEnd2(votes)
          case SequentialSpeeches(speeches) =>
            SequentialSpeeches2(speeches.mapVals(DebateSpeech2.fromSpeech))
        }

      val fullRounds =
        if (wasJudgeless) {
          Optics
            .lastOption[Vector[DebateRound2], DebateRound2]
            .modify {
              case JudgeFeedback2(distribution, feedback, endDebate) =>
                NegotiateEnd2((0 until numDebaters).map(_ -> true).toMap)
              case _ =>
                throw new NotImplementedError
            }(initRounds)
        } else
          initRounds

      fullRounds
    }

    val offlineJudgingResults: Map[String, OfflineJudgingResult] =
      if (wasJudgeless) {
        debate
          .rounds
          .lastOption
          .collect { case JudgeFeedback(distribution, feedback, endDebate) =>
            Map(
              feedback.speaker.name ->
                OfflineJudgingResult.Timed(
                  distribution,
                  SpeechSegments.getString(feedback.content),
                  feedback.timestamp,
                  timeTakenMillis = -1L
                )
            )
          }
          .getOrElse(Map())
      } else
        Map()

    Debate2(
      setup = setup,
      rounds = rounds,
      offlineJudgingResults = offlineJudgingResults,
      feedback = Map()
    )
  }
}

/** Outcome of a debate turn after some/all relevant parties have submitted
  * their arguments / info
  */
@JsonCodec
sealed trait DebateRound2 {
  def allSpeeches: Set[DebateSpeech2]

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
case class SimultaneousSpeeches2(
  speeches: Map[Int, DebateSpeech2] // map from answer index -> statement
) extends DebateRound2 {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches                  = speeches.values.toSet

}
object SimultaneousSpeeches2

@Lenses
@JsonCodec
case class SequentialSpeeches2(speeches: Map[Int, DebateSpeech2]) extends DebateRound2 {
  def isComplete(numDebaters: Int) = speeches.size == numDebaters
  def allSpeeches                  = speeches.values.toSet

}
object SequentialSpeeches2

@Lenses
@JsonCodec
case class JudgeFeedback2(
  distribution: Vector[Double], // probability distribution
  feedback: DebateSpeech2,
  endDebate: Boolean
) extends DebateRound2 {
  def isComplete(numDebaters: Int) = true
  def allSpeeches                  = Set(feedback)
}
object JudgeFeedback2

@Lenses
@JsonCodec
case class NegotiateEnd2(votes: Map[Int, Boolean]) extends DebateRound2 {
  def isComplete(numDebaters: Int) = votes.size == numDebaters
  def allSpeeches                  = Set()
}
object NegotiateEnd2

object DebateRound2 {
  val simultaneousSpeeches = GenPrism[DebateRound2, SimultaneousSpeeches2]
  val sequentialSpeeches   = GenPrism[DebateRound2, SequentialSpeeches2]
  val judgeFeedback        = GenPrism[DebateRound2, JudgeFeedback2]
  val negotiateEnd         = GenPrism[DebateRound2, NegotiateEnd2]
}

/** Specifies who gets to speak next and what kind of input they should provide.
  */
sealed trait DebateTurnType2 {
  type AllowedRole <: DebateRole
  type Round
  type Input // type of input the turn expects from a participant of one of the admissible roles
  type Out = Input // for jjm.Dot stuff
  def charLimitOpt: Option[Int]
  def quoteLimit: Option[Int]
  def currentRoles: Set[AllowedRole]

  def roundPrism: Prism[DebateRound2, Round]
  def newRoundTransition(role: AllowedRole): Input => Round
  def curRoundSpeeches(role: AllowedRole): Input => Round => Round
}
object DebateTurnType2 {
  def fromDebateTurnType(tt: DebateTurnType) =
    tt match {
      case DebateTurnType.DebaterSpeechTurn(debater, charLimit, quoteLimit) =>
        DebaterSpeechTurn(debater, charLimit, quoteLimit)
      case DebateTurnType.JudgeFeedbackTurn(reportBeliefs, charLimit, mustEndDebate) =>
        JudgeFeedbackTurn(reportBeliefs, charLimit, mustEndDebate)
      case DebateTurnType.NegotiateEndTurn(remainingDebaters) =>
        NegotiateEndTurn(remainingDebaters)
      case DebateTurnType.SimultaneousSpeechesTurn(remainingDebaters, charLimit, quoteLimit) =>
        SimultaneousSpeechesTurn(remainingDebaters, charLimit, quoteLimit)
    }

  case class SimultaneousSpeechesTurn(
    remainingDebaters: Set[Int],
    charLimit: Int,
    quoteLimit: Option[Int]
  ) extends DebateTurnType2 {
    type AllowedRole = Debater
    type Round       = SimultaneousSpeeches2
    type Input       = DebateSpeech2
    def currentRoles = remainingDebaters.map(Debater(_))
    def charLimitOpt = Some(charLimit)
    def roundPrism   = DebateRound2.simultaneousSpeeches

    def newRoundTransition(role: AllowedRole) = { case DebateSpeech2(name, timestamp, contents) =>
      SimultaneousSpeeches2(Map(role.answerIndex -> DebateSpeech2(name, timestamp, contents)))
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeech2 => Round => Round = {
      case DebateSpeech2(name, timestamp, contents) =>
        val speech = DebateSpeech2(name, timestamp, contents)
        SimultaneousSpeeches2.speeches.modify(_ + (role.answerIndex -> speech))
    }
  }
  case class DebaterSpeechTurn(debater: Int, charLimit: Int, quoteLimit: Option[Int])
      extends DebateTurnType2 {
    type AllowedRole = Debater
    type Round       = SequentialSpeeches2
    type Input       = DebateSpeech2
    def currentRoles = Set(Debater(debater))
    def charLimitOpt = Some(charLimit)
    def roundPrism   = DebateRound2.sequentialSpeeches

    def newRoundTransition(role: AllowedRole) = { case DebateSpeech2(name, timestamp, contents) =>
      SequentialSpeeches2(Map(role.answerIndex -> DebateSpeech2(name, timestamp, contents)))
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeech2 => Round => Round = {
      case DebateSpeech2(name, timestamp, contents) =>
        val speech = DebateSpeech2(name, timestamp, contents)
        SequentialSpeeches2.speeches.modify(_ + (debater -> speech))
    }
  }
  case class JudgeFeedbackTurn(reportBeliefs: Boolean, charLimit: Int, mustEndDebate: Boolean)
      extends DebateTurnType2 {
    type AllowedRole = Judge.type
    type Round       = JudgeFeedback2
    type Input       = JudgeFeedback2
    def charLimitOpt = Some(charLimit)
    def currentRoles = Set(Judge)
    def quoteLimit   = None
    def roundPrism   = DebateRound2.judgeFeedback

    def newRoundTransition(role: AllowedRole) = { judgeFeedback =>
      require(!mustEndDebate || judgeFeedback.endDebate, "Judge must end the debate.")
      judgeFeedback
    }

    def curRoundSpeeches(role: AllowedRole): JudgeFeedback2 => JudgeFeedback2 => JudgeFeedback2 =
      // no more speeches this round (shouldn't get here)
      ???
  }

  case class NegotiateEndTurn(remainingDebaters: Set[Int]) extends DebateTurnType2 {
    type AllowedRole = Debater
    type Round       = NegotiateEnd2
    type Input       = Boolean

    def charLimitOpt = None
    def quoteLimit   = None

    def currentRoles = remainingDebaters.map(Debater(_))

    def roundPrism = DebateRound2.negotiateEnd

    def newRoundTransition(role: AllowedRole) = { case votesForEnd =>
      NegotiateEnd2(Map(role.answerIndex -> votesForEnd))
    }

    def curRoundSpeeches(role: AllowedRole): Boolean => Round => Round = { case votesForEnd =>
      NegotiateEnd2.votes.modify(_ + (role.answerIndex -> votesForEnd))
    }
  }
}
