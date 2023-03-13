package debate
package scheduler

import java.nio.file.Paths

import munit.CatsEffectSuite

// import cats.effect.Blocker
// import cats.effect.IO
// import cats.effect.Resource
// import cats.implicits._

// import cats.data.Validated
// import scala.util.Random

// import jjm.implicits._
// import debate.util.DenseDistribution

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

  // val serverFixture = ResourceFixture(
  //   // "server",
  //   Resource.make(
  //     Blocker[IO].use { blocker =>
  //       Server.create(Paths.get("data"), Paths.get("scratch/save-server"), blocker)
  //     }
  //   )(_ => IO.unit)
  // )

  // override def munitFixtures = List(serverFixture)

  // serverFixture.test("We get the expected number of question matching failures") { server =>
  //   Utils.validateQualityMatches(server.qualityDataset, server.singleTurnDebateDataset) match {
  //     case Validated.Valid(_) =>
  //       ()
  //     case Validated.Invalid(violations) =>
  //       // we expect 255 violations here
  //       assertEquals(violations.size, 255L)
  //   }
  // }

  // serverFixture.test("We can pick a new story and schedule debates for it") { server =>
  //   val qualityMatches = Utils
  //     .identifyQualityMatches(server.qualityDataset, server.singleTurnDebateDataset)
  //   // val candidateStories = server.qualityDataset.filter(_._2.)

  //   val allStories = server.qualityDataset.values.view.filter(_.source == "Gutenberg").toSet

  //   server
  //     .officialDebates
  //     .rooms
  //     .get
  //     .flatMap { debates =>
  //       val allDebates     = debates.values.view.map(_.debate.debate).toVector
  //       val complete       = allDebates.filter(_.isOver).map(_.setup)
  //       val origIncomplete = allDebates.filterNot(_.isOver).map(_.setup)

  //       server
  //         .profiles
  //         .get
  //         .map { profiles =>
  //           def sampleSchedule(incomplete: Vector[DebateSetup]) = {
  //             val debatedStories = (complete ++ incomplete)
  //               .view
  //               .map(setup => SourceMaterialId.fromSourceMaterial(setup.sourceMaterial))
  //               .toSet

  //             val (nextStory, nextQAs) =
  //               allStories
  //                 .filterNot(s =>
  //                   debatedStories.contains(SourceMaterialId.QuALITYStory(s.articleId, s.title))
  //                 )
  //                 .flatMap(story => qualityMatches.get(story.articleId).map(story -> _))
  //                 .filter { case (story, matchingQids) =>
  //                   story
  //                     .questions
  //                     .filter(_._2.split == "dev")
  //                     .map(_._1)
  //                     .toSet
  //                     .intersect(matchingQids)
  //                     .nonEmpty
  //                 }
  //                 .map { case (story, _) =>
  //                   story -> DebateScheduler.getQAsForStory(story)
  //                 }
  //                 .filter(_._2.size < 6) // For now since we don't have enough debaters
  //                 .head

  //             val rand = new Random(2353250235L)

  //             val scheduleSample =
  //               DebateScheduler.efficientlySampleSchedules(
  //                 people = profiles.keySet,
  //                 complete = complete,
  //                 incomplete = incomplete,
  //                 rules = DebateRules.default,
  //                 sourceMaterial = QuALITYSourceMaterial(
  //                   articleId = nextStory.articleId,
  //                   title = nextStory.title,
  //                   contents = Server.tokenizeStory(nextStory.article)
  //                 ),
  //                 qas = nextQAs,
  //                 numDebatesPerQuestion = 2,
  //                 numOfflineJudgesPerDebate = 0,
  //                 debaters = profiles.mapVals(_ => DebaterLoadConstraint(None, None)),
  //                 creationTime = System.currentTimeMillis(),
  //                 rand
  //               ) match {
  //                 case Left(msg) =>
  //                   System.err.println(msg);
  //                   ???
  //                 case Right(res) =>
  //                   res
  //               }

  //             val scheduleDist = DenseDistribution.fromSoftmax[Schedule](scheduleSample, -_.cost)

  //             val schedule = scheduleDist.sample(rand)

  //             // println("===== QAs =====")
  //             // nextQAs.foreach(println)
  //             println("===== New rounds =====")
  //             schedule.novel.map(DebateScheduler.renderAssignmentText).foreach(println)
  //             def printLoads(debates: Vector[DebateSetup]) = debates
  //               .foldMap(setup =>
  //                 setup
  //                   .roles
  //                   .toVector
  //                   .foldMap { case (role, debater) =>
  //                     val roleStr =
  //                       role match {
  //                         case Debater(i) if i == setup.correctAnswerIndex =>
  //                           "Honest"
  //                         case Debater(_) =>
  //                           "Dishonest"
  //                         case Judge =>
  //                           "Judge"
  //                       }
  //                     Map(debater -> Map(roleStr -> 1))
  //                   }
  //               )
  //               .foreach { case (debater, roles) =>
  //                 println(s"$debater: $roles")
  //               }
  //             println("========== ALL NOVEL ==========")
  //             printLoads(schedule.novel)
  //             println("========== ALL INCOMPLETE ==========")
  //             printLoads(schedule.allIncomplete)
  //             println("========== Holistic Workload ==========")
  //             schedule
  //               .workloadCounts
  //               .foreach { case (debater, load) =>
  //                 println(f"$debater%-20s $load%.2f")
  //               }

  //             schedule
  //           }

  //           val sched1 = sampleSchedule(origIncomplete)
  //           println("==================== AGAIN! =========================")
  //           val sched2 = sampleSchedule(sched1.allIncomplete)
  //           println("==================== AGAIN! =========================")
  //           val _ = sampleSchedule(sched2.allIncomplete)
  //         }
  //     }

  // }

}
