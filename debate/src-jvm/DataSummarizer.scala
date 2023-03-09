package debate

import java.nio.file.{Path => NIOPath}

import com.github.tototoshi.csv._
import java.io.File
import cats.effect.IO

object DataSummarizer {

  // add new fields here
  val debateSummaryFields: List[(String, Debate => String)] = List(
    "Honest debater"    -> (d => d.setup.roles(Debater(d.setup.correctAnswerIndex))),
    "Dishonest debater" -> (d => d.setup.roles(Debater(1 - d.setup.correctAnswerIndex)))
  )
  def debateSummaryHeaderRow = "Room name" :: debateSummaryFields.map(_._1)

  def debateSummaryRow(name: String, debate: Debate) = name :: debateSummaryFields.map(_._2(debate))

  def writeSummaries(debates: Map[String, Debate], summaryDir: NIOPath): IO[Unit] = IO {
    val rows = debateSummaryHeaderRow :: debates.toList.map(Function.tupled(debateSummaryRow))
    CSVWriter.open(new File(summaryDir.resolve("debates.csv").toString)).writeAll(rows)
  }

}
