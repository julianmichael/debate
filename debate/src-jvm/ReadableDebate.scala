package debate

import io.circe.generic.JsonCodec
import jjm.ling.Text

@JsonCodec
case class ReadableTurn(
  role: String,
  index: Option[Int],
  text: String,
  probabilities: Option[Vector[Double]]
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

  def fromDebate(debate: Debate, userName: String, role: Role) = {
    val sourceMaterialId = SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial)
    val turnList =
      debate
        .visibleRounds(userName, role)
        .map(_.round)
        .flatMap(constructTurnsForRound(debate, _))
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

  def sessionsFromDebate(debate: Debate) = {
    val sourceMaterialId = SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial)
    val turnLists: List[(List[ReadableTurn], String, Boolean)] =
      List(
        debate
          .result
          .flatMap(_.judgingInfo)
          .map(judgingInfo =>
            (
              constructTurnsForLiveJudge(debate, debate.rounds.toList),
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
                        debate.rounds.toList
                      )
                    case OfflineJudgingMode.Timed =>
                      constructTurnsForTimedOfflineJudge(
                        debate,
                        finalJudgment,
                        debate.rounds.toList
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

  def constructTurnsForRound(debate: Debate, round: DebateRound): List[ReadableTurn] =
    round match {
      case SimultaneousSpeeches(speeches) =>
        speeches
          .toList
          .sortBy(_._1)
          .map { case (index, speech) =>
            ReadableTurn(
              role = "Debater",
              index = Some(index),
              text = SpeechSegments
                .getSpeechString(debate.setup.sourceMaterial.contents, speech.content),
              None
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
              text = SpeechSegments
                .getSpeechString(debate.setup.sourceMaterial.contents, speech.content),
              None
            )
          }
      case JudgeFeedback(dist, feedback, _) =>
        List(
          ReadableTurn(
            role = "Judge",
            index = None,
            text = SpeechSegments
              .getSpeechString(debate.setup.sourceMaterial.contents, feedback.content),
            Some(dist)
          )
        )
      case NegotiateEnd(_) =>
        Nil
      case OfflineJudgments(_) =>
        Nil
    }
  def constructTurnsForLiveJudge(debate: Debate, rounds: List[DebateRound]): List[ReadableTurn] =
    rounds.flatMap(constructTurnsForRound(debate, _))

  def constructTurnsForSteppedOfflineJudge(
    debate: Debate,
    judgments: List[JudgeFeedback],
    rounds: List[DebateRound]
  ): List[ReadableTurn] =
    judgments.map { case JudgeFeedback(distribution, feedback, _) =>
      ReadableTurn(
        role = "Offline Judge (Stepped)",
        index = None,
        text = SpeechSegments
          .getSpeechString(debate.setup.sourceMaterial.contents, feedback.content),
        Some(distribution)
      )
    } match {
      case Nil =>
        Nil
      case head :: nonFirstJudgeTurns =>
        head ::
          (rounds
            .filter {
              case SimultaneousSpeeches(_) | SequentialSpeeches(_) =>
                true;
              case _ =>
                false
            }
            .map(constructTurnsForRound(debate, _))
            .zip(nonFirstJudgeTurns)
            .flatMap(Function.tupled(_ :+ _)))
    }

  def constructTurnsForTimedOfflineJudge(
    debate: Debate,
    judgment: JudgeFeedback,
    rounds: List[DebateRound]
  ): List[ReadableTurn] =
    rounds
      .filter {
        case SimultaneousSpeeches(_) | SequentialSpeeches(_) =>
          true;
        case _ =>
          false
      }
      .flatMap(constructTurnsForRound(debate, _)) :+
      ReadableTurn(
        role = "Offline Judge (Timed)",
        index = None,
        text = SpeechSegments
          .getSpeechString(debate.setup.sourceMaterial.contents, judgment.feedback.content),
        Some(judgment.distribution)
      )

}
