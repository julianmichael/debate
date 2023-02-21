package debate.singleturn

import java.net.URL
import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._
import java.io.File

// import jjm.io.FileUtil

object SingleTurnDebateUtils {

  val debateDataName = "single_turn_debate_results.csv"
  val debateDataURL =
    "https://github.com/nyu-mll/single_turn_debate/raw/main/argument_judging_data/final_data/full_final_results.csv"

  def ensureDataIsDownloaded(dataPath: NIOPath, blocker: Blocker)(
    implicit cs: ContextShift[IO]
  ): IO[Unit] = {
    val debateDataPath = dataPath.resolve(debateDataName)
    IO(Files.exists(debateDataPath)).ifM(
      ifTrue = IO.unit,
      ifFalse =
        for {
          _ <- IO(println("Downloading single turn debate data..."))
          //   _ <- fs2.io.file.createDirectories[IO](blocker, dataPath)
          _ <-
            fs2
              .io
              .readInputStream(
                IO(new URL(debateDataURL).openConnection.getInputStream),
                4096,
                blocker,
                true
              )
              .through(fs2.io.file.writeAll(dataPath.resolve(debateDataName), blocker))
              .compile
              .drain
          _ <- IO(println("Downloaded single turn debate data."))
        } yield ()
    )
  }

  import com.github.tototoshi.csv._

  def readSingleTurnDebate(dataPath: NIOPath, blocker: Blocker)(
    implicit cs: ContextShift[IO]
  ): IO[Map[String, Vector[SingleTurnDebateQuestion]]] =
    for {
      _ <- ensureDataIsDownloaded(dataPath, blocker)
      questions <- IO {
        val reader        = CSVReader.open(new File(dataPath.resolve(debateDataName).toString))
        val questionLines = reader.allWithHeaders()
        questionLines
          .view
          .map(row =>
            SingleTurnDebateQuestion(
              passageId = row("passage_id"),
              questionText = row("question_text")
            )
          )
          .toVector
          .groupBy(_.passageId)
      }
    } yield questions
}
