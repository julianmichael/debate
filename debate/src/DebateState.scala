package debate


import monocle.macros.Lenses
import monocle.macros.GenPrism
import io.circe.generic.JsonCodec

import cats.implicits._

import jjm.DotPair
import jjm.implicits._

import monocle.function.{all => Optics}
import monocle.Prism

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
    case Some(debate) =>
      debate.currentTransitions.fold(
        _ => RoomStatus.Complete,
        _ => RoomStatus.InProgress
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

case class DebateSpeechContent(
  speakerName: String,
  timestamp: Long,
  speech: Vector[SpeechSegment]
)

case class JudgeFeedbackContent(
    speakerName: String,
    timestamp: Long,
    speech: Vector[SpeechSegment],
    distribution: Vector[Double], // probability distribution
    endDebate: Boolean
)

/** Set of operations available to a particular role. */
case class DebateTransitionSet(
  undo: Map[Role, (Vector[SpeechSegment], Debate)],
  giveSpeech: Map[Role, DotPair[Lambda[A => A => Debate], DebateTurnType]]
) {
  def currentSpeakers = giveSpeech.keySet
  def currentTurns = giveSpeech.mapVals(_.fst)
}


@Lenses @JsonCodec case class DebateResult(
    correctAnswerIndex: Int,
    numTurns: Int,
    finalJudgement: Vector[Double],
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

  /** Time of the first round of the debate (not the init time of the debate
    * setup).
    */
  def startTime: Option[Long] =
    rounds.headOption.flatMap(_.timestamp(setup.numDebaters))

  def result: Option[DebateResult] = currentTransitions.left.toOption
  def isOver: Boolean = result.nonEmpty
  def finalJudgement: Option[Vector[Double]] = result.map(_.finalJudgement)

  def numContinues = rounds.foldMap {
    case JudgeFeedback(_, _, false) => 1
    case _                          => 0
  }

  /** Whose turn(s) it is, what they can do, and how to compute the results. */
  def currentTransitions: Either[DebateResult, DebateTransitionSet] = {

    // turn sequence is always nonempty
    val numDebaters = setup.answers.size
    val roundSequence = setup.rules.roundTypes

    def newRoundSpeeches(roundType: DebateRoundType) = {
      val turn = roundType.getFirstTurn(numDebaters)
      turn.currentRoles.map(role =>
        (role: Role) -> DotPair[Lambda[I => I => Debate]](
          turn)(
          turn.newRoundTransition(role).andThen(
            round => Debate.rounds.modify(_ :+ turn.roundPrism.reverseGet(round))(this)
          )
        )
      ).toMap
    }

    // import DebateTurnType._
    def curRoundSpeeches(
      turnType: DebateTurnType
    ): Map[Role, DotPair[Lambda[I => I => Debate], DebateTurnType]] = {
      turnType.currentRoles.map(role =>
        (role: Role) -> DotPair[Lambda[I => I => Debate]](
          turnType)(
          (input: turnType.Input) => Debate.rounds
            .composeOptional(Optics.lastOption)
            .composePrism(turnType.roundPrism)
            .modify(turnType.curRoundSpeeches(role)(input))(this))
      ).toMap

    }

    // round is done, so we can create a new round with the possibility of undo
    def lastRoundUndos = rounds.lastOption.map {
      case JudgeFeedback(_, feedback, _) => 
        Map((Judge: Role) -> (feedback.content -> Debate.rounds.modify(_.init)(this)))
      case SimultaneousSpeeches(speeches) => 
        val isOnlySpeech = speeches.size == 1
        speeches.map { case (speakerIndex, speech) => 
          val newDebate = if(isOnlySpeech) {
            Debate.rounds.modify(_.init)(this)
          } else Debate.rounds
            .composeOptional(Optics.lastOption)
            .set(SimultaneousSpeeches(speeches - speakerIndex))(this)

          (Debater(speakerIndex): Role) -> (
            speech.content -> newDebate
          )
        }
      case SequentialSpeeches(speeches) =>
        val isOnlySpeech = speeches.size == 1
        speeches.map { case (speakerIndex, speech) => 
          val newDebate = if(isOnlySpeech) {
            Debate.rounds.modify(_.init)(this)
          } else Debate.rounds
            .composeOptional(Optics.lastOption)
            .set(SequentialSpeeches(speeches - speakerIndex))(this)

          (Debater(speakerIndex): Role) -> (
            speech.content -> newDebate
          )
        }
    }.getOrElse(Map())

    // TODO: validate that the debate follows the specified structure?
    if (rounds.isEmpty) Right(
      DebateTransitionSet(
        Map(),
        newRoundSpeeches(roundSequence.head)
      )
    ) else {
      val lastRoundType #:: futureRoundTypes = roundSequence.drop(rounds.size - 1)
      val lastRound = rounds.last
      lastRoundType.getTurn(lastRound, numDebaters) match {
        case DebateTurnTypeResult.Next =>
          Right(
            DebateTransitionSet(
              lastRoundUndos,
              newRoundSpeeches(futureRoundTypes.head)
            )
          )
        case DebateTurnTypeResult.Turn(turn) => Right(
          DebateTransitionSet(
            lastRoundUndos,
            curRoundSpeeches(turn)
          )
        )
        case DebateTurnTypeResult.End(finalJudgement) =>
          val numTurns = numContinues
          val judgeReward = setup.rules.scoringFunction.eval(
            numTurns,
            finalJudgement,
            setup.correctAnswerIndex
          )
          Left(
            DebateResult(
              correctAnswerIndex = setup.correctAnswerIndex,
              numTurns = numTurns,
              finalJudgement = finalJudgement,
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
  final def timestamp(numDebaters: Int): Option[Long] =
    if (!isComplete(numDebaters)) None
    else {
      allSpeeches.view.map(_.timestamp).maxOption
    }
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
object DebateRound {
  val simultaneousSpeeches = GenPrism[DebateRound, SimultaneousSpeeches]
  val sequentialSpeeches = GenPrism[DebateRound, SequentialSpeeches]
  val judgeFeedback = GenPrism[DebateRound, JudgeFeedback]
}

/** Specifies who gets to speak next and what kind of input they should provide.
  */
sealed trait DebateTurnType {
  type AllowedRole <: Role
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
    type Round = SimultaneousSpeeches
    type Input = DebateSpeechContent
    def currentRoles = remainingDebaters.map(Debater(_))
    def roundPrism = DebateRound.simultaneousSpeeches

    def newRoundTransition(role: AllowedRole) = {
      case DebateSpeechContent(name, timestamp, contents) =>
        SimultaneousSpeeches(
          Map(role.answerIndex -> DebateSpeech(
              ParticipantId(name, role),
              timestamp,
              contents
            )
          )
        )
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeechContent => Round => Round = {
      case DebateSpeechContent(name, timestamp, contents) =>
        val speech = DebateSpeech(
          ParticipantId(name, role),
          timestamp,
          contents
        )
        SimultaneousSpeeches.speeches.modify(
          _ + (role.answerIndex -> speech)
        )
    }
  }
  case class DebaterSpeechTurn(debater: Int, charLimit: Int, quoteLimit: Option[Int])
      extends DebateTurnType {
    type AllowedRole = Debater
    type Round = SequentialSpeeches
    type Input = DebateSpeechContent
    def currentRoles = Set(Debater(debater))

    def roundPrism = DebateRound.sequentialSpeeches

    def newRoundTransition(role: AllowedRole) = {
      case DebateSpeechContent(name, timestamp, contents) =>
        SequentialSpeeches(
          Map(role.answerIndex -> DebateSpeech(
              ParticipantId(name, role),
              timestamp,
              contents
            )
          )
        )
    }

    def curRoundSpeeches(role: AllowedRole): DebateSpeechContent => Round => Round = {
      case DebateSpeechContent(name, timestamp, contents) =>
        val speech = DebateSpeech(
          ParticipantId(name, Debater(debater)),
          timestamp,
          contents
        )
        SequentialSpeeches.speeches.modify(_ + (debater -> speech))
    }
  }
  case class JudgeFeedbackTurn(reportBeliefs: Boolean, charLimit: Int)
      extends DebateTurnType {
    type AllowedRole = Judge.type
    type Round = JudgeFeedback
    type Input = JudgeFeedbackContent
    def currentRoles = Set(Judge)
    def quoteLimit = None
    def roundPrism = DebateRound.judgeFeedback

    def newRoundTransition(role: AllowedRole) = {
      case JudgeFeedbackContent(name, timestamp, contents, distribution, endDebate) =>
        JudgeFeedback(
          distribution = distribution,
          DebateSpeech(
            ParticipantId(name, role),
            timestamp,
            contents
          ),
          endDebate = endDebate
        )
    }

    def curRoundSpeeches(role: AllowedRole): JudgeFeedbackContent => JudgeFeedback => JudgeFeedback = {
      // no more speeches this round (shouldn't get here)
      require(false)
      ???
    }
  }
}

@JsonCodec sealed trait SourceMaterial {
  def title: String
  def contents: Vector[String]
}
@JsonCodec case class CustomSourceMaterial(
    title: String,
    contents: Vector[String]
) extends SourceMaterial
@JsonCodec case class QuALITYSourceMaterial(
    articleId: String,
    title: String,
    contents: Vector[String]
) extends SourceMaterial
object SourceMaterial

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
    sourceMaterial: SourceMaterial,
    question: String,
    answers: Vector[String],
    correctAnswerIndex: Int,
    roles: Map[DebateRole, String],
    startTime: Long // TODO change this to initTime to distinguish it more clearly from Debate#startTime
) {
  def numDebaters = answers.size

  def areAllRolesAssigned = {
    roles.contains(Judge) && answers.indices.forall(i =>
      roles.contains(Debater(i))
    )
  }

  def assignedRole(userName: String): Option[DebateRole] =
    roles.find(_._2 == userName).map(_._1)
  def userIsAssigned(userName: String) = assignedRole(userName).nonEmpty
  def roleIsAssigned(role: Role) = role match {
    case role: DebateRole => roles.contains(role)
    case _                => false
  }
  def canAssumeRole(userName: String, role: Role) =
    assignedRole(userName) == Some(role) || (
      !roleIsAssigned(role) && !userIsAssigned(userName)
    )
}
object DebateSetup {}
