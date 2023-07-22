package debate

import io.circe.generic.JsonCodec
import jjm.ling.Text

@JsonCodec
case class ReadableTurn(
  role: String,
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
          constructTurnsForRound(
            debate,
            visibleRound.roundType,
            visibleRound.round,
            quoteDelimiters
          )
        }
        .toList
    ReadableDebate(
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

  def sessionsFromDebate(debate: Debate, quoteDelimiters: (String, String)) = {
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
                debate.setup.rules.roundTypes.zip(debate.rounds).toList,
                quoteDelimiters
              ),
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
                        debate.setup.rules.roundTypes.zip(debate.rounds).toList,
                        quoteDelimiters
                      )
                    case OfflineJudgingMode.Timed =>
                      constructTurnsForTimedOfflineJudge(
                        debate,
                        finalJudgment,
                        debate.setup.rules.roundTypes.zip(debate.rounds).toList,
                        quoteDelimiters
                      )
                  }

                (
                  turns,
                  finalJudgment.feedback.speaker,
                  finalJudgment.distribution(debate.setup.correctAnswerIndex) > 0.5
                )
              }
          )
      ).flatten

    turnLists.map { case (turnList, judge, isJudgeCorrect) =>
      ReadableDebate(
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

  def constructTurnsForRound(
    debate: Debate,
    roundType: DebateRoundType,
    round: DebateRound,
    quoteDelimiters: (String, String)
  ): List[ReadableTurn] =
    round match {
      case SimultaneousSpeeches(speeches) =>
        speeches
          .toList
          .sortBy(_._1)
          .map { case (index, speech) =>
            ReadableTurn(
              role = "Debater",
              index = Some(index),
              text = SpeechSegments.getSpeechString(
                debate.setup.sourceMaterial.contents,
                speech.content,
                quoteDelimiters
              ),
              None,
              chars = SpeechSegments
                .getSpeechLength(debate.setup.sourceMaterial.contents, speech.content),
              charLimit = roundType.charLimitOpt,
              quoteChars = SpeechSegments
                .getQuoteCoverage(debate.setup.sourceMaterial.contents, speech.content),
              quoteCharLimit = roundType.quoteLimitOpt.flatten
            )
          }
      case SequentialSpeeches(speeches) =>
        speeches
          .toList
          .sortBy(_._1)
          .map { case (index, speech) =>
            ReadableTurn(
              role = "Debater",
              index = Some(index),
              text = SpeechSegments.getSpeechString(
                debate.setup.sourceMaterial.contents,
                speech.content,
                quoteDelimiters
              ),
              None,
              chars = SpeechSegments
                .getSpeechLength(debate.setup.sourceMaterial.contents, speech.content),
              charLimit = roundType.charLimitOpt,
              quoteChars = SpeechSegments
                .getQuoteCoverage(debate.setup.sourceMaterial.contents, speech.content),
              quoteCharLimit = roundType.quoteLimitOpt.flatten
            )
          }
      case JudgeFeedback(dist, feedback, _) =>
        List(
          ReadableTurn(
            role = "Judge",
            index = None,
            text = SpeechSegments.getSpeechString(
              debate.setup.sourceMaterial.contents,
              feedback.content,
              quoteDelimiters
            ),
            Some(dist),
            chars = SpeechSegments
              .getSpeechLength(debate.setup.sourceMaterial.contents, feedback.content),
            charLimit = roundType.charLimitOpt,
            quoteChars = SpeechSegments
              .getQuoteCoverage(debate.setup.sourceMaterial.contents, feedback.content),
            quoteCharLimit = roundType.quoteLimitOpt.flatten
          )
        )
      case NegotiateEnd(_) =>
        Nil
      case OfflineJudgments(_) =>
        Nil
    }
  def constructTurnsForLiveJudge(
    debate: Debate,
    rounds: List[(DebateRoundType, DebateRound)],
    quoteDelimiters: (String, String)
  ): List[ReadableTurn] = rounds.flatMap { case (roundType, round) =>
    constructTurnsForRound(debate, roundType, round, quoteDelimiters)
  }

  def constructTurnsForSteppedOfflineJudge(
    debate: Debate,
    judgments: List[JudgeFeedback],
    rounds: List[(DebateRoundType, DebateRound)],
    quoteDelimiters: (String, String)
  ): List[ReadableTurn] =
    judgments.map { case JudgeFeedback(distribution, feedback, _) =>
      ReadableTurn(
        role = "Offline Judge (Stepped)",
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
          (rounds
            .filter {
              case (_, SimultaneousSpeeches(_)) | (_, SequentialSpeeches(_)) =>
                true;
              case _ =>
                false
            }
            .map { case (roundType, round) =>
              constructTurnsForRound(debate, roundType, round, quoteDelimiters)
            }
            .zip(nonFirstJudgeTurns)
            .flatMap(Function.tupled(_ :+ _)))
    }

  def constructTurnsForTimedOfflineJudge(
    debate: Debate,
    judgment: JudgeFeedback,
    rounds: List[(DebateRoundType, DebateRound)],
    quoteDelimiters: (String, String)
  ): List[ReadableTurn] =
    rounds
      .filter {
        case (_, SimultaneousSpeeches(_)) | (_, SequentialSpeeches(_)) =>
          true;
        case _ =>
          false
      }
      .flatMap { case (roundType, round) =>
        constructTurnsForRound(debate, roundType, round, quoteDelimiters)
      } :+
      ReadableTurn(
        role = "Offline Judge (Timed)",
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
