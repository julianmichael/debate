package debate

import java.io.File
import java.nio.file.{Path => NIOPath}

import cats.effect.IO
import cats.implicits._

import com.github.tototoshi.csv._

object DataSummarizer {

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
        "Honest debater" -> { info =>
          info.debate.setup.roles(Debater(info.debate.setup.correctAnswerIndex))
        },
        "Dishonest debater" -> { info =>
          info.debate.setup.roles(Debater(1 - info.debate.setup.correctAnswerIndex))
        },
        "Judge" -> { info =>
          info.debate.setup.roles.get(Judge).map(_.toString).getOrElse("")
        },
        "Final probability correct" -> { info =>
          info.debate.finalJudgement.map(_.apply(info.debate.setup.correctAnswerIndex).toString).getOrElse("")
        },
        "Rounds" -> { info =>
          info.debate.numContinues.toString
        },
        "Is over" -> { info => info.debate.isOver.toString },
        // TODO: conditional/option for whether or not debate.isOver
        // currently is last time debated instead?
        "End time" -> { info =>
          info.debate.rounds.view.flatMap(_.timestamp(info.debate.setup.numDebaters)).lastOption.map(_.toString).getOrElse("")
        }
      )
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
        "Room creation time" -> { info =>
          info.debate.setup.creationTime.toString
        },
        "Room name" -> { info =>
          info.roomName
        },
        "Speech time" -> { info => info.round.allSpeeches(info.role).timestamp.toString },
        "Participant" -> { info => info.round.allSpeeches(info.role).speaker },
        "Role" -> { info => info.role.toString },
        "Round index" -> { info => info.roundIndex.toString},
        "Participant text" -> { info => info.round.allSpeeches(info.role).content
          .collect{ case SpeechSegment.Text(text) => text }.toString },
        "Participant quote" -> { info => info.round.allSpeeches(info.role).content
          .collect{ case SpeechSegment.Quote(span) => Utils.renderSpan(info.debate.setup.sourceMaterial.contents, span) }.toString 
        },
        // Q: length redundant here?
        "Text length" -> { info => info.round.allSpeeches(info.role).content
          .collect{ case SpeechSegment.Text(text) => text.size }.toString },
        "Text quote" -> { info => info.round.allSpeeches(info.role).content
          .collect{ case SpeechSegment.Quote(span) => Utils.renderSpan(info.debate.setup.sourceMaterial.contents, span).size }.toString 
        },
        // "Probability correct" -> { info => info. }
      )
    }

  case class DebateSessionInfo(roomName: String, debate: Debate, participant: String)

  val sessionsCSV =
    new CSVSpec[DebateSessionInfo] {
      def name = "sessions"

      def fields: List[(String, DebateSessionInfo => String)] = List(
        "Room name" -> { info =>
          info.roomName
        },
        "Participant" -> { info =>
          info.participant
        }
        // "Feedback test" -> { info =>
        //   info.debate.feedback
        // }
      )
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
          participant <-
            debate.setup.roles.values.toSet ++ debate.setup.offlineJudges.keySet ++
              debate.offlineJudgingResults.keySet
        } yield DebateSessionInfo(roomName, debate, participant)

      sessionsCSV.writeToPath(sessionInfos, summaryDir)
    }

}
