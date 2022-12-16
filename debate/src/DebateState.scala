package debate

import monocle.macros.Lenses
import io.circe.generic.JsonCodec

import cats.implicits._
import jjm.DotPair

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
  undo: Option[() => Debate],
  giveSpeech: Option[DotPair[Lambda[A => A => Debate], DebateTurnType]]
)

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

  /** Time of the first round of the debate (not the init time of the debate
    * setup).
    */
  def startTime: Option[Long] =
    rounds.headOption.flatMap(_.timestamp(setup.numDebaters))

  def result: Option[DebateResult] = currentTransitions.left.toOption
  def isOver: Boolean = result.nonEmpty
  def finalJudgment: Option[Vector[Double]] = result.map(_.finalJudgment)

  def numContinues = rounds.foldMap {
    case JudgeFeedback(_, _, false) => 1
    case _                          => 0
  }

  def whoCanUndo: Set[Role] = {
    rounds.lastOption match {
      case None => Set()
      case Some(round) =>
        round match {
          case JudgeFeedback(_, _, _) => Set(Judge)
          case SequentialSpeeches(speeches) =>
            speeches.lastOption match {
              case None =>
                Set() // TODO maybe-someday let the judge undo here- requires dropping two rounds in [DebatePanel.scala]
              case Some((_, speech)) => Set(speech.speaker.role)
            }
          case SimultaneousSpeeches(speeches) =>
            (speeches.map({ case (_, speech) => speech.speaker.role })).toSet
        }
    }
  }

  /** Whose turn(s) it is, what they can do, and how to compute the results. */
  def currentTransitions: Either[DebateResult, Map[Role, DebateTransitionSet]] = {

    // turn sequence is always nonempty
    val numDebaters = setup.answers.size
    val roundSequence = setup.rules.roundTypes

    def newRound(undoResult: Option[() => Debate]) = Right {
      val firstTurn = roundSequence.head.getFirstTurn(numDebaters)
      firstTurn.rolesRemaining.map(role =>
        (role: Role) -> DebateTransitionSet(
          undo = undoResult,
          giveSpeech = Some(
            DotPair[Lambda[I => I => Debate]](
              firstTurn)(
              firstTurn.newRoundTransition(role).andThen(
                round => Debate.rounds.modify(_ :+ round)(this)
              )
            )
          )
        )
      ).toMap
    }

    // TODO: validate that the debate follows the specified structure?
    if (rounds.isEmpty) newRound(None) else {
      val lastRoundType #:: futureRoundTypes = roundSequence.drop(rounds.size - 1)
      val lastRound = rounds.last
      lastRoundType.getTurn(lastRound, numDebaters) match {
        case DebateTurnTypeResult.Next => newRound(???) // TODO get undo info
        case DebateTurnTypeResult.Turn(turn) => Right(???) // TODO get turn change info
        case DebateTurnTypeResult.End(finalJudgment) =>
          val numTurns = numContinues
          val judgeReward = setup.rules.scoringFunction.eval(
            numTurns,
            finalJudgment,
            setup.correctAnswerIndex
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

// These exist for @Lenses
object JudgeFeedback

/** Specifies who gets to speak next and what kind of input they should provide.
  */
sealed trait DebateTurnType {
  type AllowedRole <: Role
  type Input // type of input the turn expects from a participant of one of the admissible roles
  type Out = Input // for jjm.Dot stuff
  def charLimit: Int
  def quoteLimit: Option[Int]
  def rolesRemaining: Set[AllowedRole]

  def newRoundTransition(role: AllowedRole): Input => DebateRound
}
object DebateTurnType {
  case class SimultaneousSpeechesTurn(
      remainingDebaters: Set[Int],
      charLimit: Int,
      quoteLimit: Option[Int]
  ) extends DebateTurnType {
    type AllowedRole = Debater
    type Input = DebateSpeechContent
    def rolesRemaining = remainingDebaters.map(Debater(_))

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
  }
  case class DebaterSpeechTurn(debater: Int, charLimit: Int, quoteLimit: Option[Int])
      extends DebateTurnType {
    type AllowedRole = Debater
    type Input = DebateSpeechContent
    def rolesRemaining = Set(Debater(debater))

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
  }
  case class JudgeFeedbackTurn(reportBeliefs: Boolean, charLimit: Int)
      extends DebateTurnType {
    type AllowedRole = Judge.type
    type Input = JudgeFeedbackContent
    def rolesRemaining = Set(Judge)
    def quoteLimit = None

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
object DebateSetup {
}
