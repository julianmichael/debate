package debate

import java.io.File
import java.nio.file.{Path => NIOPath}

import cats.effect.IO
import cats.implicits._

import com.github.tototoshi.csv._

import jjm.metrics.Numbers

import debate.quality.QuALITYStory

class DataSummarizer(qualityDataset: Map[String, QuALITYStory]) {

  sealed trait CSVSpec[A] {

    def name: String

    def fields: List[(String, A => String)]

    def writeToPath(items: List[A], path: NIOPath): IO[Unit] = IO {
      val headerRow = fields.map(_._1)

      val rows = headerRow :: items.toList.map(item => fields.map(_._2(item)))
      CSVWriter.open(new File(path.resolve(s"$name.csv").toString)).writeAll(rows)
    }
  }

  case class DebateInfo(roomName: String, debate: Debate)

  val debatesCSV =
    new CSVSpec[DebateInfo] {
      def name = "debates"

      def fields: List[(String, DebateInfo => String)] = List(
        "Creation time" -> { info =>
          info.debate.setup.creationTime.toString
        },
        "Start time" -> { info =>
          info.debate.startTime.map(_.toString).getOrElse("")
        },
        "Room name" -> (_.roomName),
        "Is offline" -> { info =>
          (!info.debate.setup.rules.hasJudge).toString
        },

        // TODO: Ask J, best for merge w/ questsion metadata here or in .py
        // re: IDK if this is the best way
        "Article ID" -> { info =>
          info.debate.setup.sourceMaterial match {
            case QuALITYSourceMaterial(articleId, _, _) =>
              articleId

            case _ =>
              ""
          }
        },
        "Question" -> { info =>
          info.debate.setup.question
        },
        "Speed annotator accuracy" -> { info =>
          info.debate.setup.sourceMaterial match {
            case QuALITYSourceMaterial(articleId, _, _) =>
              val story = qualityDataset(articleId)
              story
                .questions
                .find(_._2.question == info.debate.setup.question)
                .map(_._2)
                .flatMap(_.annotations)
                .map(_.speedAccuracyAgainstGold.toString)
                .getOrElse("")
            case _ =>
              ""
          }
        },
        // TO maybe DO: more setup parameters? But we've stuck to the same scoring rule & char limit etc for this semester right...
        "Correct answer" -> { info =>
          info.debate.setup.answers(info.debate.setup.correctAnswerIndex)
        },
        "Wrong answer" -> { info =>
          info.debate.setup.answers(1 - info.debate.setup.correctAnswerIndex)
        },
        "Honest debater" -> { info =>
          info.debate.setup.roles(Debater(info.debate.setup.correctAnswerIndex))
        },
        "Dishonest debater" -> { info =>
          info.debate.setup.roles(Debater(1 - info.debate.setup.correctAnswerIndex))
        },
        "Debater A" -> { info =>
          info.debate.setup.roles(Debater(0))
        },
        "Debater B" -> { info =>
          info.debate.setup.roles(Debater(1))
        },

        // TO maybe DO: more than one judge? => for now num offline judges and their average probability correct
        "Judge" -> { info =>
          info.debate.setup.roles.get(Judge).map(_.toString).getOrElse("")
        },
        "Num offline judges" -> { info =>
          info.debate.setup.offlineJudges.keySet.size.toString
        },
        "Final probability correct" -> { info =>
          info
            .debate
            .finalJudgement
            .map(_.apply(info.debate.setup.correctAnswerIndex).toString)
            .getOrElse("")
        },
        "Average offline probability correct" -> { info =>
          info
            .debate
            .offlineJudgingResults
            .values
            .toList
            .toNel
            .map(
              _.reduceMap(j =>
                  j.result
                    .foldMap(res => Numbers(res.distribution(info.debate.setup.correctAnswerIndex)))
                )
                .stats
                .mean
                .toString
            )
            .getOrElse("")
        },
        "Number of debate rounds" -> { info =>
          info
            .debate
            .rounds
            .collect {
              case SimultaneousSpeeches(_) =>
                ()
              case SequentialSpeeches(_) =>
                ()
            }
            .size
            .toString
        },
        "Number of continues" -> { info =>
          info.debate.numContinues.toString
        },
        "Status" -> { info =>
          info.debate.status match {
            case RoomStatus.WaitingToBegin =>
              "waiting"
            case RoomStatus.InProgress =>
              "in progress"
            case RoomStatus.Complete(_, _, _) =>
              "complete"
          }
        },
        "Is over" -> { info =>
          info.debate.isOver.toString
        },
        // TO maybe DO: conditional/option for whether or not debate.isOver for "End time"
        // currently is last time debated instead?
        "End time" -> { info =>
          info.debate.rounds.view.flatMap(_.maxTimestamp).lastOption.map(_.toString).getOrElse("")
        }
      )
      // TODO: anything else missing for overall debate room info?
    }

  case class DebateTurnInfo(
    roomName: String,
    debate: Debate,
    round: DebateRound,
    roundIndex: Int,
    role: DebateRole,
    speech: DebateSpeech
  )

  val turnsCSV =
    new CSVSpec[DebateTurnInfo] {
      def name = "turns"

      def fields: List[(String, DebateTurnInfo => String)] = List(
        "Room name" -> { info =>
          info.roomName
        },
        "Room start time" -> { info =>
          info.debate.setup.creationTime.toString
        },
        "Speech time" -> { info =>
          info.round.allSpeeches(info.role).timestamp.toString
        },
        "Participant" -> { info =>
          info.round.allSpeeches(info.role).speaker
        },
        "Role" -> { info =>
          info.role.toString
        },
        "Round index" -> { info =>
          info.roundIndex.toString
        },
        "Num previous judging rounds" -> { info =>
          info
            .debate
            .rounds
            .take(info.roundIndex)
            .collect { case JudgeFeedback(_, _, _) =>
              ()
            }
            .size
            .toString
        },
        "Num previous debating rounds" -> { info =>
          info
            .debate
            .rounds
            .take(info.roundIndex)
            .collect {
              case SequentialSpeeches(_) =>
                ()
              case SimultaneousSpeeches(_) =>
                ()
            }
            .size
            .toString
        },
        // TODO: ask, should we have one whole speech text instead? Should we separate with something other than <<quote>> / [[input text]]?
        "Participant text" -> { info =>
          info
            .round
            .allSpeeches(info.role)
            .content
            .collect { case SpeechSegment.Text(text) =>
              text
            }
            .mkString(" <<quote>> ")
        },
        "Participant quote" -> { info =>
          info
            .round
            .allSpeeches(info.role)
            .content
            .collect { case SpeechSegment.Quote(span) =>
              Utils.renderSpan(info.debate.setup.sourceMaterial.contents, span)
            }
            .mkString(" [[input text]] ")
        },
        "Text length" -> { info =>
          info
            .round
            .allSpeeches(info.role)
            .content
            .collect { case SpeechSegment.Text(text) =>
              text
            }
            .foldMap(_.size)
            .toString
        },
        "Quote length" -> { info =>
          info
            .round
            .allSpeeches(info.role)
            .content
            .collect { case SpeechSegment.Quote(span) =>
              Utils.renderSpan(info.debate.setup.sourceMaterial.contents, span)
            }
            .foldMap(_.size)
            .toString
        },
        // Q: saw you had a function for this but wasnt sure how or if I should use it
        // "Textlength" -> { info => info.round.allSpeeches(info.role).content.flatMap(_.getSpeechLength).toString }, ??

        "Probability correct" -> { info =>
          info.round match {
            case JudgeFeedback(distribution, _, _) =>
              assert(info.role == Judge)
              distribution(info.debate.setup.correctAnswerIndex).toString
            case _ =>
              ""
          }
        }
      )
    }

  // Q: maybe just feedback instead of "debatesession" would be a better name
  // Seems like feedback is the only info here anyway? And session doesn't really clarify debateroom-participant idea
  case class DebateSessionInfo(
    roomName: String,
    debate: Debate,
    role: DebateRole,
    participant: String
  )

  val sessionsCSV =
    new CSVSpec[DebateSessionInfo] {
      def name = "sessions"

      def normalFields: List[(String, DebateSessionInfo => String)] = List(
        "Room name" -> { info =>
          info.roomName
        },
        "Room start time" -> { info =>
          info.debate.setup.creationTime.toString
        },
        "Participant" -> { info =>
          info.participant
        },
        "Role" -> { info =>
          info.role.toString
        },
        "Is turn" -> { info =>
          info.debate.stateInfo._2.currentSpeakers.contains(info.role).toString
        },
        "Is over" -> { info =>
          info.debate.isOver.toString
        }

        // Q: how many times debated / judged could possibly be derived from making a timeline of the debates in .py
        // But there might be cases where times overlap.. let's decide def first?
      )

      def surveyFields: List[(String, DebateSessionInfo => String)] = {
        val surveyQuestions = Feedback.questions.keySet.toList
        surveyQuestions.flatMap {
          // Separate columns for comparative likert answers first and second
          case key @ Feedback.Key.ComparativeLikert(keyname) =>
            List(
              keyname + ".1" -> { info: DebateSessionInfo =>
                info
                  .debate
                  .feedback
                  .get(info.participant)
                  .flatMap(
                    _.answers
                      .get(key)
                      .collectFirst {
                        case Feedback.ComparativeJudgment(first, _) if first > -1 =>
                          first.toString
                      }
                  )
                  .getOrElse("")
              },
              keyname + ".2" -> { info: DebateSessionInfo =>
                info
                  .debate
                  .feedback
                  .get(info.participant)
                  .flatMap(
                    _.answers
                      .get(key)
                      .collectFirst {
                        case Feedback.ComparativeJudgment(_, second) if second > -1 =>
                          second.toString
                      }
                  )
                  .getOrElse("")
              }
            )
          case key @ Feedback.Key.Likert(keyname) =>
            List(
              keyname -> { info: DebateSessionInfo =>
                info
                  .debate
                  .feedback
                  .get(info.participant)
                  .flatMap(_.answers.get(key).map(_.toString))
                  .getOrElse("")
              }
            )
          case key @ Feedback.Key.FreeText(keyname) =>
            List(
              keyname -> { info: DebateSessionInfo =>
                info
                  .debate
                  .feedback
                  .get(info.participant)
                  .flatMap(_.answers.get(key).map(_.toString))
                  .getOrElse("")
              }
            )
          case key @ Feedback.Key.RoleSelect(keyname) =>
            LiveDebateRole
              .allRoles(numDebaters = 2)
              .flatMap { role =>
                List(
                  s"$keyname.${role.toString}" -> { info: DebateSessionInfo =>
                    info
                      .debate
                      .feedback
                      .get(info.participant)
                      .flatMap(_.answers.get(key))
                      .flatMap(_.get(role))
                      .getOrElse("")
                  }
                )
              }
          case _ =>
            List.empty[(String, DebateSessionInfo => String)]
        }
      }

      // Combine normal fields and survey fields
      def fields: List[(String, DebateSessionInfo => String)] = normalFields ::: surveyFields

    }

  def writeSummaries(debates: Map[String, Debate], summaryDir: NIOPath) =
    debatesCSV.writeToPath(
      debates
        .toList
        .map { case (roomName, debate) =>
          DebateInfo(roomName, debate)
        },
      summaryDir
    ) >> {
      val turnInfos =
        for {
          (roomName, debate) <- debates.toList
          roles = Judge :: (0 until debate.numDebaters).map(Debater(_)).toList
          (round, roundIndex) <- debate.rounds.zipWithIndex
          role                <- roles
          turn <-
            (round, role) match {
              case (JudgeFeedback(_, speech, _), Judge) =>
                Some(DebateTurnInfo(roomName, debate, round, roundIndex, role, speech))
              case (SimultaneousSpeeches(speeches), Debater(i)) =>
                speeches
                  .get(i)
                  .map(speech => DebateTurnInfo(roomName, debate, round, roundIndex, role, speech))
              case (SequentialSpeeches(speeches), Debater(i)) =>
                speeches
                  .get(i)
                  .map(speech => DebateTurnInfo(roomName, debate, round, roundIndex, role, speech))
              case _ =>
                None
            }
        } yield turn
      turnsCSV.writeToPath(turnInfos, summaryDir)
    } >> {
      val sessionInfos =
        for {
          (roomName, debate) <- debates.toList
          (role, participant) <-
            debate.setup.roles.toSet ++ debate.setup.offlineJudges.keySet.map(OfflineJudge -> _) ++
              debate.offlineJudgingResults.keySet.map(OfflineJudge -> _)
        } yield DebateSessionInfo(roomName, debate, role, participant)

      sessionsCSV.writeToPath(sessionInfos, summaryDir)
    }

}
