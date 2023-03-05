package debate
package scheduler

import java.nio.file.Paths

import cats.effect.Blocker
import cats.effect.IO
import cats.effect.Resource
import cats.implicits._

import munit.CatsEffectSuite

import cats.data.Validated
import scala.util.Random

import jjm.implicits._

class LiveSchedulerTests extends CatsEffectSuite {
  val dataPath = Paths.get("data")

//   val qualityDataset = ResourceSuiteLocalFixture(
//     "quality",
//     Resource
//       .make(Blocker[IO].use(blocker => QuALITYUtils.readQuALITY(dataPath, blocker)))(_ => IO.unit)
//   )

//   val singleTurnDebateDataset = ResourceSuiteLocalFixture(
//     "singleturn",
//     Resource.make(
//       Blocker[IO].use(blocker => SingleTurnDebateUtils.readSingleTurnDebate(dataPath, blocker))
//     )(_ => IO.unit)
//   )

  val serverFixture = ResourceSuiteLocalFixture(
    "server",
    Resource.make(
      Blocker[IO].use { blocker =>
        Server.create(Paths.get("data"), Paths.get("scratch/save-server"), blocker)
      }
    )(_ => IO.unit)
  )

  override def munitFixtures = List(serverFixture)

  test("We get the expected number of question matching failures") {
    val server = serverFixture()
    Utils.validateQualityMatches(server.qualityDataset, server.singleTurnDebateDataset) match {
      case Validated.Valid(_) =>
        ()
      case Validated.Invalid(violations) =>
        // we expect 255 violations here
        assertEquals(violations.size, 255L)
    }
  }

  test("We can pick a new story and schedule debates for it") {
    val server = serverFixture()
    val qualityMatches = Utils
      .identifyQualityMatches(server.qualityDataset, server.singleTurnDebateDataset)
    // val candidateStories = server.qualityDataset.filter(_._2.)

    server
      .officialDebates
      .rooms
      .get
      .flatMap { debates =>
        server
          .profiles
          .get
          .map { profiles =>
            val debatedStories =
              debates
                .view
                .map(d =>
                  SourceMaterialId.fromSourceMaterial(d._2.debate.debate.setup.sourceMaterial)
                )
                .toSet
            val allStories = server.qualityDataset.values.view.filter(_.source == "Gutenberg").toSet
            val nextStory =
              allStories
                .filterNot(s =>
                  debatedStories.contains(SourceMaterialId.QuALITYStory(s.articleId, s.title))
                )
                .flatMap(story => qualityMatches.get(story.articleId).map(story -> _))
                .filter { case (story, matchingQids) =>
                  story
                    .questions
                    .filter(_._2.split == "dev")
                    .map(_._1)
                    .toSet
                    .intersect(matchingQids)
                    .nonEmpty
                }
                .head
                ._1

            val nextQAs =
              nextStory
                .questions
                .values
                .view
                .flatMap { question =>
                  for {
                    annotations <- question.annotations
                    if annotations.goldLabel == annotations.writerLabel
                    if annotations.untimedAccuracyAgainstGold == 1.0
                    if annotations.speedAccuracyAgainstGold <= 0.5
                    bestDistractors = annotations
                      .bestDistractors(correctAnswerIndex = annotations.goldLabel)
                  } yield {
                    if (bestDistractors.size > 1) {
                      import io.circe.syntax._
                      System.err.println(s"Multiple best distractors: ${question.asJson.spaces2}")
                    }
                    DebateScheduler.QASpec(
                      question = question.question,
                      correctAnswer = question.options(annotations.goldLabel - 1),
                      incorrectAnswer = question.options(bestDistractors.head - 1)
                    )
                  }
                }
                .toVector

            val allDebates = debates.values.view.map(_.debate.debate).toVector

            val scheduleDist =
              DebateScheduler
                .getDebateScheduleDistribution(
                  debates = allDebates,
                  rules = DebateRules.default,
                  sourceMaterial = QuALITYSourceMaterial(
                    articleId = nextStory.articleId,
                    title = nextStory.title,
                    contents = Server.tokenizeStory(nextStory.article)
                  ),
                  qas = nextQAs,
                  numDebatesPerQuestion = 2,
                  numOfflineJudgesPerDebate = 2,
                  debaters = profiles.mapVals(_ => DebaterLoadConstraint(None, None)),
                  creationTime = System.currentTimeMillis()
                )
                .get

            val rand = new Random(2353250235L)

            val schedule = scheduleDist.sample(rand)

            println("===== QAs =====")
            nextQAs.foreach(println)
            println("===== New rounds =====")
            println(schedule.novel)
          }
      }
  }

}
