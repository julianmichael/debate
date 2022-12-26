package debate.quality

import java.net.URL
import java.nio.file.{Path => NIOPath}
import java.nio.file.Files

import cats.effect.Blocker
import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._

import fs2.Stream

import jjm.io.FileUtil

object QuALITYUtils {

  val qualityDataName = "QuALITY.v1.0.1"
  val qualityURL =
    "https://github.com/nyu-mll/quality/blob/main/data/v1.0.1/QuALITY.v1.0.1.zip?raw=true"
  // val qualityDataPath = Paths.get("data/QuALITY.v1.0.1")

  def ensureQualityIsDownloaded(dataPath: NIOPath, blocker: Blocker)(
    implicit cs: ContextShift[IO]
  ): IO[Unit] = {
    val qualityPath = dataPath.resolve(qualityDataName)
    IO(Files.isDirectory(qualityPath)).ifM(
      ifTrue = IO.unit,
      ifFalse =
        for {
          _ <- IO(println("Downloading QuALITY data..."))
          _ <- fs2.io.file.createDirectories[IO](blocker, qualityPath)
          _ <-
            fs2
              .io
              .readInputStream(
                IO(new URL(qualityURL).openConnection.getInputStream),
                4096,
                blocker,
                true
              )
              .through(fs2.io.file.writeAll(dataPath.resolve(qualityDataName + ".zip"), blocker))
              .compile
              .drain
          _ <- IO(println("Downloaded QuALITY."))
          // too lazy to unzip in fs2
          _ <- IO(
            os.proc("unzip", "../" + qualityDataName + ".zip")
              .call(cwd = os.pwd / dataPath.toString / qualityDataName)
          )
        } yield ()
    )
  }

  def cleanStoryText(story: String): String = {
    val newlineStandin = "###NEWLINE####"
    story
      .replaceAll("\n\n\n+", "\n\n")
      .replaceAll("\n\n", newlineStandin)
      .replaceAll("\n", " ")
      .replaceAll(newlineStandin, "\n\n")
  }

  def readQuALITY(dataPath: NIOPath, blocker: Blocker)(
    implicit cs: ContextShift[IO]
  ): IO[Map[String, QuALITYStory]] =
    for {
      _ <- ensureQualityIsDownloaded(dataPath, blocker)
      allInstances <-
        Stream
          .emits[IO, String](List("train", "dev", "test"))
          .flatMap { split =>
            val filename = s"$qualityDataName.htmlstripped.$split"
            val filePath = dataPath.resolve(qualityDataName).resolve(filename)
            FileUtil
              .readJsonLines[QuALITYInstance](filePath)
              .map(_.toStory(split))
              .map(QuALITYStory.article.modify(cleanStoryText))
          }
          .compile
          .toVector
      instancesByArticleId = allInstances.toList.groupByNel(_.articleId)
      storiesByArticleId <- instancesByArticleId
        .toVector
        .traverse { case (articleId, instances) =>
          IO.fromEither(instances.reduceLeftMonadic(_ merge _).map(articleId -> _))
        }
        .map(_.toMap)
    } yield storiesByArticleId

}
