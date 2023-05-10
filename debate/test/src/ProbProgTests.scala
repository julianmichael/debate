package debate

import munit.CatsEffectSuite

import cats.implicits._

import com.stripe.rainier.core._
import com.stripe.rainier.compute._
import com.stripe.rainier.sampler._
import cats.effect.Blocker
import cats.effect.IO
import java.nio.file.Paths
import jjm.implicits._

class ProbProgTests extends CatsEffectSuite {

  test("Babby's first variable") {
    val a = Uniform(0, 1).latent
    assert(clue(Model.sample(a).take(10)).forall(_ >= 0))
  }

  val eggs = List[Long](
    45, 52, 45, 47, 41, 42, 44, 42, 46, 38, 36, 35, 41, 48, 42, 29, 45, 43, 45, 40, 42, 53, 31, 48,
    40, 45, 39, 29, 45, 42
  )
  val lambda   = Gamma(0.5, 100).latent
  val eggModel = Model.observe(eggs, Poisson(lambda))

  test("Babby's first MAP") {
    val result = eggModel.optimize(lambda)
    assert(clue(result) > 0)
  }

  test("Babby's first sample") {
    val sampler  = EHMC(5000, 500)
    val eggTrace = eggModel.sample(sampler)
    eggTrace
      .diagnostics
      .foreach { diagnostics =>
        assert(clue(diagnostics.rHat) < 1.1)
        assert(clue(diagnostics.effectiveSampleSize) == 2000.0)
      }
  }

  test("Babby's first posterior") {
    val sampler   = EHMC(5000, 500)
    val eggTrace  = eggModel.sample(sampler)
    val posterior = eggTrace.predict(lambda)
    assert(clue(posterior.meanOpt).exists(m => math.abs(m - 42.0) < 2.0))
  }

  def getQuestionId(debate: Debate) = {
    val storyId  = SourceMaterialId.fromSourceMaterial(debate.setup.sourceMaterial)
    val question = debate.setup.question
    storyId -> question
  }

  case class DebateData(debates: Vector[Debate], debaters: Vector[String]) {
    val questions = debates.map(getQuestionId).toSet.toVector
  }

  def loadDebateData: IO[DebateData] = Blocker[IO].use { blocker =>
    for {
      server   <- Server.create(Paths.get("data"), Paths.get("save"), blocker)
      debates  <- server.officialDebates.rooms.get.map(_.values.toVector.map(_.debate.debate))
      debaters <- server.profiles.get.map(_.keySet.toVector)
    } yield DebateData(debates, debaters)
  }

  implicit def zip5toGen[A, B, C, D, E, Z, Y, X, W, V](
    implicit az: ToGenerator[A, Z],
    by: ToGenerator[B, Y],
    cx: ToGenerator[C, X],
    dw: ToGenerator[D, W],
    ev: ToGenerator[E, V]
  ): ToGenerator[(A, B, C, D, E), (Z, Y, X, W, V)] =
    new ToGenerator[(A, B, C, D, E), (Z, Y, X, W, V)] {
      def apply(t: (A, B, C, D, E)) = az(t._1)
        .zip(by(t._2))
        .zip(cx(t._3))
        .zip(dw(t._4))
        .zip(ev(t._5))
        .map { case ((((z, y), x), w), v) =>
          (z, y, x, w, v)
        }
    }

  // test("Babby's first debater ELO model") {
  //   val data         = loadDebateData.unsafeRunSync()
  //   val globalBias   = Normal(0, 1).latent
  //   val questionEase = Normal(0, 1).latentVec(data.questions.size)
  //   val debaterSkill = Normal(0, 1).latentVec(data.debaters.size)
  //   val judgeSkill   = Normal(0, 1).latentVec(data.debaters.size)
  //   // val debateVariance = Gamma(0.5, 100).latent
  //   val offlineAdjustment = Normal(0, 1).latent

  //   val personIndex   = data.debaters.zipWithIndex.toMap
  //   val questionIndex = data.questions.zipWithIndex.toMap

  //   val allVariablesModel = Model.track(
  //     Set(globalBias) ++ questionEase.toVector ++ debaterSkill.toVector ++ judgeSkill.toVector
  //   )

  //   val onlineModel = {
  //     case class OnlineResult(debate: Debate, judging: JudgingResult, index: Int) {
  //       val probCorrect  = judging.finalJudgement(judging.correctAnswerIndex)
  //       def logitCorrect = math.log10(probCorrect / (1 - probCorrect))
  //     }
  //     val onlineResults = data
  //       .debates
  //       .zipWithIndex
  //       .flatMap { case (debate, index) =>
  //         debate
  //           .result
  //           .flatMap(_.judgingInfo)
  //           .map { judgingInfo =>
  //             OnlineResult(debate, judgingInfo, index)
  //           }
  //       }
  //     val onlineResultLogits = onlineResults.map(_.logitCorrect)
  //     val onlineResultModels = onlineResults.map { case OnlineResult(debate, _, _) =>
  //       val setup            = debate.setup
  //       val roles            = setup.roles
  //       val thisQuestionEase = questionEase(questionIndex(getQuestionId(debate)))
  //       val thisHonestSkill  = debaterSkill(personIndex(roles(Debater(setup.correctAnswerIndex))))
  //       val thisDishonestSkill = debaterSkill(
  //         personIndex(roles(Debater(1 - setup.correctAnswerIndex)))
  //       )
  //       val thisJudgeSkill = judgeSkill(personIndex(roles(Judge)))
  //       val logit =
  //         globalBias + thisQuestionEase + thisHonestSkill - thisDishonestSkill + thisJudgeSkill
  //       logit
  //     }
  //     Model.observe(onlineResultLogits, Vec.from(onlineResultModels).map(Normal(_, 1.0)))
  //   }

  //   val offlineModel = {
  //     // same as online model, but looking at offlineJudgments for each debate
  //     case class OfflineResult(
  //       debate: Debate,
  //       judge: String,
  //       offlineJudgment: OfflineJudgingResult,
  //       index: Int
  //     ) {
  //       val probCorrect  = offlineJudgment.distribution(debate.setup.correctAnswerIndex)
  //       def logitCorrect = math.log10(probCorrect / (1 - probCorrect))
  //     }
  //     val offlineResults = data
  //       .debates
  //       .zipWithIndex
  //       .flatMap { case (debate, index) =>
  //         debate
  //           .offlineJudgingResults
  //           .toVector
  //           .flatMap { case (judge, judgment) =>
  //             judgment.result.map(result => OfflineResult(debate, judge, result, index))
  //           }
  //       }
  //     val offlineResultLogits = offlineResults.map(_.logitCorrect)
  //     val offlineResultModels = offlineResults.map { case OfflineResult(debate, judge, _, _) =>
  //       val setup            = debate.setup
  //       val roles            = setup.roles
  //       val thisQuestionEase = questionEase(questionIndex(getQuestionId(debate)))
  //       val thisHonestSkill  = debaterSkill(personIndex(roles(Debater(setup.correctAnswerIndex))))
  //       val thisDishonestSkill = debaterSkill(
  //         personIndex(roles(Debater(1 - setup.correctAnswerIndex)))
  //       )
  //       val thisJudgeSkill = judgeSkill(personIndex(judge))
  //       val logit =
  //         globalBias + thisQuestionEase + thisHonestSkill - thisDishonestSkill + thisJudgeSkill +
  //           offlineAdjustment
  //       logit
  //     }

  //     Model.observe(offlineResultLogits, Vec.from(offlineResultModels).map(Normal(_, 1.0)))
  //   }

  //   val fullModel = List(onlineModel, offlineModel, allVariablesModel).reduce(_ merge _)

  //   val map = fullModel
  //     .optimize((globalBias, offlineAdjustment, questionEase, debaterSkill, judgeSkill))

  //   def mkElo(x: Double) = 1200 + (x * 400)

  //   def sigmoid10(x: Double) = 1 / (1 + math.pow(10, -x))

  //   def renderElos(skills: Seq[Double]) = skills
  //     .zipWithIndex
  //     .sortBy(-_._1)
  //     .map { case (skill, i) =>
  //       f"\t${data.debaters(i)}%s (${mkElo(skill).toInt}%d) — ${math.pow(10, skill)}%.2f:1 (${sigmoid10(skill)}%.2f))"
  //     }
  //     .mkString("\n")

  //   val avgElos = map._4.lazyZip(map._5).map(_ + _).map(_ / 2)

  //   val res =
  //     f"""
  //        |Global bias (${mkElo(map._1)}) — ${math.pow(10, map._1)}%.2f:1 (${sigmoid10(map._1)}%.2f)
  //        |Offline adjustment: (${mkElo(map._2)}) — ${math
  //         .pow(10, map._2)}%.2f:1 (${sigmoid10(map._2)}%.2f)
  //        |Debater Elos:\n${renderElos(map._4)}%s
  //        |Judge Elos:\n${renderElos(map._5)}%s
  //        |Average Elos:\n${renderElos(avgElos)}%s
  //        |""".stripMargin.trim

  //   println(res)

  //   assert(true)

  // }

  test("Uncommon sense accuracy model") {
    val numQuestions = 200
    // expert accuracy
    // val consistency         = Beta(1, 1).latent
    // val questionConsistency = Binomial(consistency).latentVec(numQuestions)

    // non-expert accuracy
    val questionEaseMean = Uniform(0.5, 0.7).latent
    // not sure why the MAP bunches up against the upper bound, but it does.
    // Anyway, 4.0 gets us reasonable looking statistics over samples.
    val questionEasePrecision = Uniform(0.5, 4.0).latent
    val questionEase = Beta
      .meanAndPrecision(questionEaseMean, questionEasePrecision)
      .latentVec(numQuestions)

    val numAnnotators = 3

    val numCorrectCounts = Map(0 -> 0.15, 1 -> 0.2, 2 -> 0.35, 3 -> 0.3)
      .mapVals(x => (x * 200).toInt)

    val instances =
      numCorrectCounts
        .view
        .flatMap { case (numCorrect, count) =>
          Vector.fill(count)(numCorrect.toLong)
        }
        .zip(questionEase.toVector)
        .toVector

    val model = Model
      .observe(instances.map(_._1), Vec.from(instances.map(_._2)).map(Binomial(_, numAnnotators)))

    val mapEstimate = model.optimize((questionEaseMean, questionEasePrecision, questionEase))

    // val posterior = model.posterior((easeMean, easeVariance, questionEase))

    println(s"""
               |MAP estimates:
               |Ease mean: ${mapEstimate._1}
               |Ease precision: ${mapEstimate._2}
               |Question ease: ${mapEstimate._3.map("\n\t" + _)}
               |""".stripMargin.trim)

    val questionDist = Beta.meanAndPrecision(mapEstimate._1, mapEstimate._2)
    val question     = questionDist.latent
    // val question = Beta(2.4, 1.6).latent

    val numCorrectAnnotations = Generator(Binomial(question, 3))
    val preds                 = Model.sample(numCorrectAnnotations).take(1000)
    println(s"Question prior: $questionDist")
    println(preds.counts.mapVals(_.toDouble / 1000).toVector.sortBy(_._1).mkString("\n"))
  }
}
