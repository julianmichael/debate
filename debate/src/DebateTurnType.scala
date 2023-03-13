package debate

import monocle.Prism

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
