package debate

import java.nio.file.{Path => NIOPath}

import com.github.tototoshi.csv._
import java.io.File
import cats.effect.IO

object DataSummarizer {

  val debateSummaryFields: List[(String, Debate => String)] = List(
    "Honest debater"    -> (d => d.setup.roles(Debater(d.setup.correctAnswerIndex))),
    "Dishonest debater" -> (d => d.setup.roles(Debater(1 - d.setup.correctAnswerIndex))),
    // TODO better map, get offline judge, or >1 judges
    "Judge"         -> (d => d.setup.roles.get(Judge).map(_.toString).getOrElse("[offline]")),
    "Creation time" -> (d => d.setup.creationTime.toString),
    "Start time"    -> (d => d.startTime.map(_.toString).getOrElse(""))
  )
  def debateSummaryHeaderRow = "Room name" :: debateSummaryFields.map(_._1)

  def debateSummaryRow(name: String, debate: Debate) = name :: debateSummaryFields.map(_._2(debate))

  def writeSummaries(debates: Map[String, Debate], summaryDir: NIOPath): IO[Unit] = IO {
    val rows = debateSummaryHeaderRow :: debates.toList.map(Function.tupled(debateSummaryRow))
    CSVWriter.open(new File(summaryDir.resolve("debates.csv").toString)).writeAll(rows)
  }

}
