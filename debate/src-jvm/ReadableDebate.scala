package debate

import io.circe.generic.JsonCodec
import jjm.ling.Text

@JsonCodec
case class ReadableTurn(
  role: String,
  speaker: String,
  index: Option[Int],
  text: String,
  probabilities: Option[Vector[Double]],
  chars: Int,
  charLimit: Option[Int],
  quoteChars: Int,
  quoteCharLimit: Option[Int]
)

@JsonCodec
case class ReadableDebate(
  roomName: String,
  storyId: String,
  storyTitle: String,
  story: String,
  question: String,
  answers: Vector[String],
  debateId: String,
  judge: String,
  turns: List[ReadableTurn],
  isJudgeCorrect: Option[Boolean]
)
object ReadableDebate {

  def fromDebate(
    roomName: String,
    debate: Debate,
    userName: String,
    role: Role,
    quoteDelimiters: (String, String)
  ) = {
    val sourceMaterialId = SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial)
    val turnList =
      debate
        .visibleRounds(userName, role)
        .flatMap { visibleRound =>
          constructTurnsForRound(debate, visibleRound, quoteDelimiters)
        }
        .toList
    ReadableDebate(
      roomName = roomName,
      storyId =
        sourceMaterialId match {
          case SourceMaterialId.QuALITYStory(id, _) =>
            id
          case _ =>
            ??? // only work for QuALITY stories
        },
      storyTitle = sourceMaterialId.title,
      story = Text.render(debate.setup.sourceMaterial.contents),
      question = debate.setup.question,
      answers = debate.setup.answers,
      debateId = debate.hashCode().toString,
      judge = debate.setup.roles.getOrElse(Judge, ""),
      turns = turnList,
      isJudgeCorrect = None
    )
  }

  def sessionsFromDebate(
    roomName: String,
    debate: Debate,
    quoteDelimiters: (String, String),
    liveOnly: Boolean
  ) = {
    val sourceMaterialId = SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial)
    val turnLists: List[(List[ReadableTurn], String, Boolean)] =
      List(
        debate
          .result
          .flatMap(_.judgingInfo)
          .map(judgingInfo =>
            (
              constructTurnsForLiveJudge(
                debate,
                debate.visibleRounds("Judge", Judge), // user name doesn't matter
                quoteDelimiters
              ).toList,
              debate.setup.roles(Judge),
              judgingInfo.finalJudgement(debate.setup.correctAnswerIndex) > 0.5
            )
          )
          .toList,
        debate
          .stateInfo
          ._3
          .values
          .toList
          .flatMap(offlineJudgment =>
            offlineJudgment
              .result
              .map { finalJudgment =>
                val turns =
                  offlineJudgment.mode match {
                    case OfflineJudgingMode.Stepped =>
                      constructTurnsForSteppedOfflineJudge(
                        debate,
                        offlineJudgment.judgments.toList,
                        quoteDelimiters
                      ).toList
                    case OfflineJudgingMode.Timed =>
                      constructTurnsForTimedOfflineJudge(debate, finalJudgment, quoteDelimiters)
                        .toList
                  }

                (
                  turns,
                  finalJudgment.feedback.speaker,
                  finalJudgment.distribution(debate.setup.correctAnswerIndex) > 0.5
                )
              }
          )
          .filter(_ => !liveOnly)
      ).flatten

    turnLists.map { case (turnList, judge, isJudgeCorrect) =>
      ReadableDebate(
        roomName = roomName,
        storyId =
          sourceMaterialId match {
            case SourceMaterialId.QuALITYStory(id, _) =>
              id
            case _ =>
              ??? // only work for QuALITY stories
          },
        storyTitle = sourceMaterialId.title,
        story = Text.render(debate.setup.sourceMaterial.contents),
        question = debate.setup.question,
        answers = debate.setup.answers,
        debateId = debate.hashCode().toString,
        judge = judge,
        turns = turnList,
        isJudgeCorrect = Some(isJudgeCorrect)
      )
    }
  }

  def getRoleIndex(role: Role): Option[Int] =
    role match {
      case Debater(i) =>
        Some(i)
      case _ =>
        None
    }

  def constructTurnsForRound(
    debate: Debate,
    visibleRound: Debate.VisibleRound,
    quoteDelimiters: (String, String)
  ): List[ReadableTurn] = visibleRound
    .visibleSpeeches
    .toList
    .sortBy(p => getRoleIndex(p._1))
    .map { case (role, speech) =>
      val roleStr =
        role match {
          case Debater(i) =>
            "Debater"
          case _ =>
            role.toString
        }
      val distOpt = DebateRound.judgeFeedback.getOption(visibleRound.round).map(_.distribution)
      ReadableTurn(
        role = roleStr,
        speaker = speech.speaker,
        index = getRoleIndex(role),
        text = SpeechSegments
          .getSpeechString(debate.setup.sourceMaterial.contents, speech.content, quoteDelimiters),
        distOpt,
        chars = SpeechSegments
          .getSpeechLength(debate.setup.sourceMaterial.contents, speech.content),
        charLimit = visibleRound.roundType.charLimitOpt,
        quoteChars = SpeechSegments
          .getQuoteCoverage(debate.setup.sourceMaterial.contents, speech.content),
        quoteCharLimit = visibleRound.roundType.quoteLimitOpt.flatten
      )
    }

  def constructTurnsForLiveJudge(
    debate: Debate,
    rounds: Vector[Debate.VisibleRound],
    quoteDelimiters: (String, String)
  ): Vector[ReadableTurn] = rounds.flatMap { round =>
    constructTurnsForRound(debate, round, quoteDelimiters)
  }

  def constructTurnsForSteppedOfflineJudge(
    debate: Debate,
    judgments: List[JudgeFeedback],
    quoteDelimiters: (String, String)
  ): List[ReadableTurn] =
    judgments.map { case JudgeFeedback(distribution, feedback, _) =>
      ReadableTurn(
        role = "Offline Judge (Stepped)",
        speaker = feedback.speaker,
        index = None,
        text = SpeechSegments
          .getSpeechString(debate.setup.sourceMaterial.contents, feedback.content, quoteDelimiters),
        Some(distribution),
        chars = SpeechSegments
          .getSpeechLength(debate.setup.sourceMaterial.contents, feedback.content),
        charLimit = None,
        quoteChars = SpeechSegments
          .getQuoteCoverage(debate.setup.sourceMaterial.contents, feedback.content),
        quoteCharLimit = None
      )
    } match {
      case Nil =>
        Nil
      case head :: nonFirstJudgeTurns =>
        head ::
          debate
            .visibleRounds("Offline Judge", OfflineJudge)
            .filter(
              _.round match {

                case SimultaneousSpeeches(_) | SequentialSpeeches(_) =>
                  true
                case _ =>
                  false
              }
            )
            .map { case round =>
              constructTurnsForRound(debate, round, quoteDelimiters)
            }
            .zip(nonFirstJudgeTurns)
            .flatMap(Function.tupled(_ :+ _))
            .toList
    }

  // TODO: missing the turns apparently
  def constructTurnsForTimedOfflineJudge(
    debate: Debate,
    judgment: JudgeFeedback,
    quoteDelimiters: (String, String)
  ): Vector[ReadableTurn] =
    debate
      .visibleRounds("Offline Judge", OfflineJudge)
      .filter(
        _.round match {
          case SimultaneousSpeeches(_) | SequentialSpeeches(_) =>
            true
          case _ =>
            false
        }
      )
      .flatMap { round =>
        constructTurnsForRound(debate, round, quoteDelimiters)
      } :+
      ReadableTurn(
        role = "Offline Judge (Timed)",
        speaker = judgment.feedback.speaker,
        index = None,
        text = SpeechSegments.getSpeechString(
          debate.setup.sourceMaterial.contents,
          judgment.feedback.content,
          quoteDelimiters
        ),
        Some(judgment.distribution),
        chars = SpeechSegments
          .getSpeechLength(debate.setup.sourceMaterial.contents, judgment.feedback.content),
        charLimit = None,
        quoteChars = SpeechSegments
          .getQuoteCoverage(debate.setup.sourceMaterial.contents, judgment.feedback.content),
        quoteCharLimit = None
      )

}
