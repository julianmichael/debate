package debate

import java.nio.file.{Path => NIOPath}

import com.github.tototoshi.csv._
import java.io.File
import cats.effect.IO

object DataSummarizer {

  val debateSummaryFields: List[(String, Debate => Any)] = List( // TODO ask J, Any ok? or split
    "Honest debater"    -> (d => d.setup.roles(Debater(d.setup.correctAnswerIndex))),
    "Dishonest debater" -> (d => d.setup.roles(Debater(1 - d.setup.correctAnswerIndex))),

    // TODO better map, get offline judge, or >1 judges. For offline vs online comparison
    "Judge" -> (d => d.setup.roles.get(Judge).map(_.toString).getOrElse("[offline]")),

    // TODO ask J, other than Debate, can other input objects be used? For 1st time judging story vs 2nd time
    // "Judge" -> (stats.allJudging - Assigned - Begun).unorderedFoldMap(_.size),

    // TODO ask J, refactor interface code for probabilities per turn? finalJudgement => last turn, end debate
    // For per turn accuracy
    "Final probability correct" ->
      (d => d.finalJudgement.map(_.apply(d.setup.correctAnswerIndex)).getOrElse("NA")),

    // TODO probably clean up first?, or as above, get separated automatically?
    // "Transcript" -> (d => d.rounds), // for per turn probabilities? for counting characters?

    "Creation time" -> (d => d.setup.creationTime.toString),
    "Start time"    -> (d => d.startTime.map(_.toString).getOrElse("")),
    "Rounds"        -> (d => d.numContinues.toString)
  )
  def debateSummaryHeaderRow = "Room name" :: debateSummaryFields.map(_._1)

  def debateSummaryRow(name: String, debate: Debate) = name :: debateSummaryFields.map(_._2(debate))

  def writeSummaries(debates: Map[String, Debate], summaryDir: NIOPath): IO[Unit] = IO {
    val rows = debateSummaryHeaderRow :: debates.toList.map(Function.tupled(debateSummaryRow))
    CSVWriter.open(new File(summaryDir.resolve("debates.csv").toString)).writeAll(rows)
  }

}
