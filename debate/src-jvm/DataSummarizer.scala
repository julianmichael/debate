package debate

import java.io.File
import java.nio.file.{Path => NIOPath}

import cats.effect.IO
import cats.implicits._

import com.github.tototoshi.csv._

import jjm.metrics.Numbers

import debate.quality.QuALITYStory
import jjm.ling.Text

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
        "Is single debater" -> { info =>
          (info.debate.setup.roles.keySet.filter(_.isDebater).size == 1).toString
        },
        "Has honest debater" -> { info =>
          info
            .debate
            .setup
            .roles
            .keySet
            .contains(Debater(info.debate.setup.correctAnswerIndex))
            .toString
        },
        "Has dishonest debater" -> { info =>
          info
            .debate
            .setup
            .roles
            .keySet
            .contains(Debater(1 - info.debate.setup.correctAnswerIndex))
            .toString
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
        "Story length (tok)" -> { info =>
          info.debate.setup.sourceMaterial match {
            case QuALITYSourceMaterial(_, _, contents) =>
              contents.size.toString()
            case _ =>
              ""
          }
        },
        "Story length (char)" -> { info =>
          info.debate.setup.sourceMaterial match {
            case QuALITYSourceMaterial(_, _, contents) =>
              Text.render(contents).size.toString()
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
        "Untimed annotator answerability" -> { info =>
          info.debate.setup.sourceMaterial match {
            case QuALITYSourceMaterial(articleId, _, _) =>
              val story = qualityDataset(articleId)
              story
                .questions
                .find(_._2.question == info.debate.setup.question)
                .map(_._2)
                .flatMap(_.annotations)
                // takes first mode...
                .map(annotation =>
                  annotation.answerability.groupBy(identity).maxBy(_._2.size)._1.toString
                ).getOrElse("")
            case _ =>
              ""
          }
        },
        "Untimed annotator context" -> { info =>
          info.debate.setup.sourceMaterial match {
            case QuALITYSourceMaterial(articleId, _, _) =>
              val story = qualityDataset(articleId)
              story
                .questions
                .find(_._2.question == info.debate.setup.question)
                .map(_._2)
                .flatMap(_.annotations)
                .map(annotation => Numbers(annotation.context).stats.mean.toString)
                .getOrElse("")
            case _ =>
              ""
          }
        },
        "Untimed annotator context (rounded)" -> { info =>
          info.debate.setup.sourceMaterial match {
            case QuALITYSourceMaterial(articleId, _, _) =>
              val story = qualityDataset(articleId)
              story
                .questions
                .find(_._2.question == info.debate.setup.question)
                .map(_._2)
                .flatMap(_.annotations)
                .map(annotation =>
                  math.round(Numbers(annotation.context).stats.mean).toInt.toString
                )
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
          info.debate.setup.roles.getOrElse(Debater(info.debate.setup.correctAnswerIndex), "")
        },
        "Dishonest debater" -> { info =>
          info.debate.setup.roles.getOrElse(Debater(1 - info.debate.setup.correctAnswerIndex), "")
        },
        "Debater A" -> { info =>
          info.debate.setup.roles.getOrElse(Debater(0), "")
        },
        "Debater B" -> { info =>
          info.debate.setup.roles.getOrElse(Debater(1), "")
        },
        "Judge" -> { info =>
          info.debate.setup.roles.getOrElse(Judge, "")
        },
        "Num offline judges" -> { info =>
          info.debate.setup.offlineJudges.keySet.size.toString
        },
        // TODO: three final probabilities: live, avg offline, avg all
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
            .flatMap(_.result)
            .toNel
            .map(
              _.reduceMap(res => Numbers(res.distribution(info.debate.setup.correctAnswerIndex)))
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
          info
            .debate
            .rounds
            .view
            .filterNot(_.isInstanceOf[OfflineJudgments])
            .flatMap(_.maxTimestamp)
            .lastOption
            .map(_.toString)
            .getOrElse("")
        },
        "Last modified time" -> { info =>
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
          info.speech.timestamp.toString
        },
        "Participant" -> { info =>
          info.speech.speaker
        },
        "Role" -> { info =>
          info.role.toString
        },
        "Role (honest/dishonest)" -> { info =>
          info.role match {
            case Debater(answerIndex) =>
              if (answerIndex == info.debate.setup.correctAnswerIndex) {
                "Honest debater"
              } else {
                "Dishonest debater"
              }
            case r =>
              r.toString
          }
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
            .speech
            .content
            .collect { case SpeechSegment.Text(text) =>
              text
            }
            .mkString(" <<quote>> ")
        },
        "Participant quote" -> { info =>
          info
            .speech
            .content
            .collect { case SpeechSegment.Quote(span) =>
              Utils.renderSpan(info.debate.setup.sourceMaterial.contents, span)
            }
            .mkString(" [[input text]] ")
        },
        "Participant quote span" -> { info =>
          info
            .speech
            .content
            .collect { case SpeechSegment.Quote(span) =>
              s"<<${span.begin}-${span.end}>>"
            }
            .mkString(" ")
        },
        "Text length" -> { info =>
          info
            .speech
            .content
            .collect {
              case SpeechSegment.Text(text) =>
                text
              case _ =>
                ""
            }
            .foldMap(_.size)
            .toString
        },
        "Quote length (tok)" -> { info =>
          info
            .speech
            .content
            .collect {
              case SpeechSegment.Quote(span) =>
                Utils.renderSpan(info.debate.setup.sourceMaterial.contents, span)
              case _ =>
                ""
            }
            .foldMap(_.size)
            .toString
        },
        // "Quote length (char)" -> { info =>
        //   SpeechSegments
        //     .getQuoteCoverage(info.debate.setup.sourceMaterial.contents, info.speech.content)
        //     .toString()
        // },
        "Probability correct" -> { info =>
          info.round match {
            case JudgeFeedback(distribution, _, _) =>
              assert(info.role == Judge)
              distribution(info.debate.setup.correctAnswerIndex).toString
            case OfflineJudgments(judgments) =>
              judgments
                .get(info.speech.speaker)
                .flatMap(_.judgments.find(_.feedback == info.speech))
                .map(_.distribution(info.debate.setup.correctAnswerIndex))
                .foldMap(_.toString)
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
          (
            info.debate.currentTransitions.currentSpeakers.contains(info.role) &&
              (info.role != OfflineJudge ||
                info.debate.offlineJudgingResults.get(info.participant).exists(_.result.isEmpty))
          ).toString
        },
        "Is over" -> { info =>
          info.debate.isOver.toString
        },
        "Number of judge continues" -> { info =>
          info.role match {
            case Judge =>
              info.debate.numContinues.toString
            case OfflineJudge =>
              info
                .debate
                .offlineJudgingResults
                .get(info.participant)
                .filter(_.result.isDefined)
                .map { judgment =>
                  judgment.mode match {
                    case OfflineJudgingMode.Stepped =>
                      judgment.judgments.size - 1
                    case OfflineJudgingMode.Timed =>
                      info
                        .debate
                        .rounds
                        .collect { case SimultaneousSpeeches(_) | SequentialSpeeches(_) =>
                          ()
                        }
                        .size
                  }
                }
                .foldMap(_.toString)
            case _ =>
              ""
          }
        },
        "Final probability correct" -> { info =>
          info.role match {
            case Judge =>
              info
                .debate
                .finalJudgement
                .map(_.apply(info.debate.setup.correctAnswerIndex).toString)
                .getOrElse("")
            case OfflineJudge =>
              info
                .debate
                .offlineJudgingResults
                .get(info.participant)
                .flatMap(_.result)
                .map(res => res.distribution(info.debate.setup.correctAnswerIndex).toString)
                .getOrElse("")
            case _ =>
              ""
          }
        },
        "Offline judging start time" -> { info =>
          info.role match {
            case OfflineJudge =>
              info
                .debate
                .offlineJudgingResults
                .get(info.participant)
                .map(_.startTimeMillis.toString)
                .getOrElse("")
            case _ =>
              ""
          }
        },
        // TO ask: stepped? is this the final?
        "Offline judging end time" -> { info =>
          info.role match {
            case OfflineJudge =>
              info
                .debate
                .offlineJudgingResults
                .get(info.participant)
                .flatMap(_.result)
                .map(res => res.feedback.timestamp.toString)
                .getOrElse("")
            case _ =>
              ""
          }
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
          roles = OfflineJudge :: Judge :: (0 until debate.numDebaters).map(Debater(_)).toList
          (round, roundIndex) <- debate.rounds.zipWithIndex
          role                <- roles
          turn <-
            (round, role) match {
              case (JudgeFeedback(_, speech, _), Judge) =>
                List(DebateTurnInfo(roomName, debate, round, roundIndex, role, speech))
              case (OfflineJudgments(speeches), OfflineJudge) =>
                speeches
                  .values
                  .toList
                  .flatMap { judgment =>
                    val debateRoundIndices =
                      debate
                        .rounds
                        .zipWithIndex
                        .collect { case (SimultaneousSpeeches(_) | SequentialSpeeches(_), i) =>
                          i
                        }
                        .toList
                    val offlineJudgeRounds =
                      judgment.mode match {
                        case OfflineJudgingMode.Stepped =>
                          judgment.judgments.zip(0 :: debateRoundIndices.map(_ + 1))
                        case OfflineJudgingMode.Timed =>
                          judgment.judgments.lastOption.map(_ -> roundIndex).toVector
                      }
                    offlineJudgeRounds.map { case (judgment, roundIndex) =>
                      DebateTurnInfo(roomName, debate, round, roundIndex, role, judgment.feedback)
                    }
                  }
              case (SimultaneousSpeeches(speeches), Debater(i)) =>
                speeches
                  .get(i)
                  .toList
                  .map(speech => DebateTurnInfo(roomName, debate, round, roundIndex, role, speech))
              case (SequentialSpeeches(speeches), Debater(i)) =>
                speeches
                  .get(i)
                  .toList
                  .map(speech => DebateTurnInfo(roomName, debate, round, roundIndex, role, speech))
              case _ =>
                List()
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
    } >> {
      val filteredDebates = AnalyzeResults.getDebatesFilteredForTime(debates.toList, qualityDataset)
      val balancedDebates = AnalyzeResults.balanceDebates(filteredDebates)

      import java.io.File
      IO(
        CSVWriter
          .open(new File(summaryDir.resolve("sample-rooms.csv").toString))
          .writeAll(
            List("Room name", "Judge") ::
              balancedDebates.map(d => List(d.roomName, d.debate.setup.roles(Judge)))
          )
      ) >>
        IO(
          CSVWriter
            .open(new File(summaryDir.resolve("sample-error-cases.csv").toString))
            .writeAll(
              List("Setting", "Room name", "Confidence", "Reason") ::
                balancedDebates
                  .filter(_.probabilityCorrect <= .5)
                  .map(d =>
                    List(
                      d.setting.show,
                      d.roomName,
                      f"${math.max(d.probabilityCorrect, 1 - d.probabilityCorrect)}%.2f",
                      d.debate
                        .feedback
                        .get(d.debate.setup.roles(Judge))
                        .flatMap(_.answers.get(Feedback.Key.FreeText("reason for outcome")))
                        .combineAll
                    )
                  )
            )
        )
    }

}
