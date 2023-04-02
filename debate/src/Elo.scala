package debate

import cats.implicits._
import jjm.implicits._

import com.stripe.rainier.core._
import com.stripe.rainier.compute._
import io.circe.generic.JsonCodec

object Elo {

  def mkElo(x: Double): Int = math.round(1200 + (x * 400)).toInt

  def sigmoid10(x: Double) = 1 / (1 + math.pow(10, -x))
  def sigmoid2(x: Double)  = 1 / (1 + math.pow(2, -x))

  // def renderElos(skills: Seq[Double]) = skills
  //   .zipWithIndex
  //   .sortBy(-_._1)
  //   .map { case (skill, i) =>
  //     f"\t${debaters(i)}%s (${mkElo(skill).toInt}%d) — ${math.pow(10, skill)}%.2f:1 (${sigmoid10(skill)}%.2f))"
  //   }
  //   .mkString("\n")

  // val avgElos = map._4.lazyZip(map._5).map(_ + _).map(_ / 2)

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

  implicit def zip6toGen[A, B, C, D, E, F, Z, Y, X, W, V, U](
    implicit az: ToGenerator[A, Z],
    by: ToGenerator[B, Y],
    cx: ToGenerator[C, X],
    dw: ToGenerator[D, W],
    ev: ToGenerator[E, V],
    fu: ToGenerator[F, U]
  ): ToGenerator[(A, B, C, D, E, F), (Z, Y, X, W, V, U)] =
    new ToGenerator[(A, B, C, D, E, F), (Z, Y, X, W, V, U)] {
      def apply(t: (A, B, C, D, E, F)) = az(t._1)
        .zip(by(t._2))
        .zip(cx(t._3))
        .zip(dw(t._4))
        .zip(ev(t._5))
        .zip(fu(t._6))
        .map { case (((((z, y), x), w), v), u) =>
          (z, y, x, w, v, u)
        }
    }

  @JsonCodec
  case class Ratings(
    globalBias: Double,
    offlineAdjustment: Double,
    questionEase: Vector[(QuestionId, Double)],
    honestSkills: Map[String, Double],
    dishonestSkills: Map[String, Double],
    judgeSkills: Map[String, Double]
  ) {
    def averageSkills = ((honestSkills |+| dishonestSkills).mapVals(_ / 2) |+| judgeSkills)
      .mapVals(_ / 2)
  }
  object Ratings {
    def empty = Ratings(0.0, 0.0, Vector(), Map(), Map(), Map())
  }

  def computeRatings(
    debates: Vector[Debate],
    debaterSet: Set[String],
    timeCutoff: Option[Long] = None
  ): Ratings = {
    val questions = debates.map(QuestionId.fromDebate)
    val debaters  = debaterSet.toVector

    // val data         = loadDebateData.unsafeRunSync()
    val globalBias     = Normal(0, 1).latent
    val questionEase   = Normal(0, 1).latentVec(questions.size)
    val honestSkill    = Normal(0, 1).latentVec(debaters.size)
    val dishonestSkill = Normal(0, 1).latentVec(debaters.size)
    val judgeSkill     = Normal(0, 1).latentVec(debaters.size)
    // val debateVariance = Gamma(0.5, 100).latent
    val offlineAdjustment = Normal(0, 1).latent

    val personIndex   = debaters.zipWithIndex.toMap
    val questionIndex = questions.zipWithIndex.toMap

    val onlineModelOpt = {
      case class OnlineResult(debate: Debate, judging: JudgingResult, index: Int) {
        val probCorrect  = judging.finalJudgement(judging.correctAnswerIndex)
        def logitCorrect = math.log(probCorrect / (1 - probCorrect)) / math.log(2.0)
      }
      val onlineResults = debates
        .zipWithIndex
        .flatMap { case (debate, index) =>
          debate
            .result
            .filter(r => timeCutoff.forall(r.timestamp <= _))
            .flatMap(_.judgingInfo)
            .map { judgingInfo =>
              OnlineResult(debate, judgingInfo, index)
            }
        }
        .filter(_.debate.setup.roles.size == 3)
      val onlineResultLogits = onlineResults.map(_.logitCorrect)
      val onlineResultModels = onlineResults.map { case OnlineResult(debate, _, _) =>
        val setup            = debate.setup
        val roles            = setup.roles
        val thisQuestionEase = questionEase(questionIndex(QuestionId.fromDebate(debate)))
        val thisHonestSkill  = honestSkill(personIndex(roles(Debater(setup.correctAnswerIndex))))
        val thisDishonestSkill = dishonestSkill(
          personIndex(roles(Debater(1 - setup.correctAnswerIndex)))
        )
        val thisJudgeSkill = judgeSkill(personIndex(roles(Judge)))
        val logit =
          globalBias + thisQuestionEase + thisHonestSkill - thisDishonestSkill + thisJudgeSkill
        logit
      }
      if (onlineResultLogits.isEmpty)
        None
      else
        Some(Model.observe(onlineResultLogits, Vec.from(onlineResultModels).map(Normal(_, 1.0))))
    }

    val offlineModelOpt = {
      // same as online model, but looking at offlineJudgments for each debate
      case class OfflineResult(
        debate: Debate,
        judge: String,
        offlineJudgment: OfflineJudgingResult,
        index: Int
      ) {
        val probCorrect  = offlineJudgment.distribution(debate.setup.correctAnswerIndex)
        def logitCorrect = math.log(probCorrect / (1 - probCorrect)) / math.log(2.0)
      }
      val offlineResults = debates
        .zipWithIndex
        .flatMap { case (debate, index) =>
          debate
            .offlineJudgingResults
            .toVector
            .flatMap { case (judge, judgment) =>
              judgment
                .result
                .filter(r => timeCutoff.forall(r.timestamp <= _))
                .map(result => OfflineResult(debate, judge, result, index))
            }
        }
        .filter(r => (r.debate.setup.roles - Judge).size == 2)
      val offlineResultLogits = offlineResults.map(_.logitCorrect)
      val offlineResultModels = offlineResults.map { case OfflineResult(debate, judge, _, _) =>
        val setup            = debate.setup
        val roles            = setup.roles
        val thisQuestionEase = questionEase(questionIndex(QuestionId.fromDebate(debate)))
        val thisHonestSkill  = honestSkill(personIndex(roles(Debater(setup.correctAnswerIndex))))
        val thisDishonestSkill = dishonestSkill(
          personIndex(roles(Debater(1 - setup.correctAnswerIndex)))
        )
        val thisJudgeSkill = judgeSkill(personIndex(judge))
        val logit =
          globalBias + thisQuestionEase + thisHonestSkill - thisDishonestSkill + thisJudgeSkill +
            offlineAdjustment
        logit
      }

      if (offlineResultLogits.isEmpty)
        None
      else
        Some(Model.observe(offlineResultLogits, Vec.from(offlineResultModels).map(Normal(_, 1.0))))
    }

    val allVariablesModel = Model.track(
      Set(globalBias, offlineAdjustment) ++ questionEase.toVector ++ honestSkill.toVector ++
        dishonestSkill.toVector ++ judgeSkill.toVector
    )

    if (onlineModelOpt.isEmpty && offlineModelOpt.isEmpty)
      return Ratings.empty
    else {

      val fullModel = (List(onlineModelOpt, offlineModelOpt).flatten ++ List(allVariablesModel))
        .reduce(_ merge _)

      val mapEstimate = fullModel.optimize(
        (globalBias, offlineAdjustment, questionEase, honestSkill, dishonestSkill, judgeSkill)
      )

      Ratings(
        mapEstimate._1,
        mapEstimate._2,
        questions.zip(mapEstimate._3),
        debaters.zip(mapEstimate._4).toMap,
        debaters.zip(mapEstimate._5).toMap,
        debaters.zip(mapEstimate._6).toMap
      )
    }

  }
}
