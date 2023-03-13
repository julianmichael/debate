package debate

import java.io.File
import java.nio.file.{Path => NIOPath}

import cats.effect.IO

import com.github.tototoshi.csv._

object DataSummarizer {

  val debateSummaryFields: List[(String, Debate => String)] = List(
    "Honest debater"    -> (d => d.setup.roles(Debater(d.setup.correctAnswerIndex))),
    "Dishonest debater" -> (d => d.setup.roles(Debater(1 - d.setup.correctAnswerIndex))),
    "Creation time"     -> (d => d.setup.creationTime.toString),
    "Start time"        -> (d => d.startTime.map(_.toString).getOrElse(""))
  )
  def debateSummaryHeaderRow = "Room name" :: debateSummaryFields.map(_._1)

  def debateSummaryRow(name: String, debate: Debate) = name :: debateSummaryFields.map(_._2(debate))

  def writeSummaries(debates: Map[String, Debate], summaryDir: NIOPath): IO[Unit] = IO {
    val rows = debateSummaryHeaderRow :: debates.toList.map(Function.tupled(debateSummaryRow))
    CSVWriter.open(new File(summaryDir.resolve("debates.csv").toString)).writeAll(rows)
  }

}
