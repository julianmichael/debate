package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import io.circe.generic.JsonCodec

import jjm.io.FileUtil
import jjm.ling.Text

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object WriteDebates
    extends CommandIOApp(
      name = "mill -i debate.jvm.runMain debate.WriteDebates",
      header = "Write the debates into a more LM-readable format."
    ) {

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "directory path",
      help = "Directory from which to read the debates."
    )
    .withDefault(Paths.get("save"))

  val outO = Opts
    .option[NIOPath](
      "out",
      metavar = "file path",
      help = "file in which to save the LM-readable debates."
    )
    .withDefault(Paths.get("debates-readable.jsonl"))

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
    isJudgeCorrect: Boolean
  )
  object ReadableDebate {
    def sessionsFromDebate(debate: Debate) = {
      val sourceMaterialId = SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial)
      def constructTurnsForRound(round: DebateRound): List[ReadableTurn] =
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
      def constructTurnsForLiveJudge(rounds: List[DebateRound]): List[ReadableTurn] = rounds
        .flatMap(constructTurnsForRound)

      def constructTurnsForSteppedOfflineJudge(
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
                .map(constructTurnsForRound)
                .zip(nonFirstJudgeTurns)
                .flatMap(Function.tupled(_ :+ _)))
        }

      def constructTurnsForTimedOfflineJudge(
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
          .flatMap(constructTurnsForRound) :+
          ReadableTurn(
            role = "Offline Judge (Timed)",
            index = None,
            text = SpeechSegments
              .getSpeechString(debate.setup.sourceMaterial.contents, judgment.feedback.content),
            Some(judgment.distribution)
          )

      val turnLists: List[(List[ReadableTurn], String, Boolean)] =
        List(
          debate
            .result
            .flatMap(_.judgingInfo)
            .map(judgingInfo =>
              (
                constructTurnsForLiveJudge(debate.rounds.toList),
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
                          offlineJudgment.judgments.toList,
                          debate.rounds.toList
                        )
                      case OfflineJudgingMode.Timed =>
                        constructTurnsForTimedOfflineJudge(finalJudgment, debate.rounds.toList)
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
          isJudgeCorrect = isJudgeCorrect
        )
      }
    }
  }

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = (saveO, outO).mapN { (save, out) =>
    Blocker[IO].use { blocker =>
      for {
        server <- Server.create(Paths.get("data"), save, blocker)
        rooms  <- server.officialDebates.rooms.get
        _ <- {
          val readableDebates =
            rooms
              .view
              .flatMap { case (_, room) =>
                ReadableDebate.sessionsFromDebate(room.debate.debate)
              }
              .toList
          FileUtil.writeJsonLines(out)(readableDebates)
        }
      } yield ExitCode.Success
    }
  }

}
