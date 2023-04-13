package debate

import cats.implicits._
import jjm.implicits._

import com.stripe.rainier.core._
import com.stripe.rainier.compute._
import io.circe.generic.JsonCodec
import cats.data.State

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

  implicit def zip7toGen[A, B, C, D, E, F, G, Z, Y, X, W, V, U, T](
    implicit az: ToGenerator[A, Z],
    by: ToGenerator[B, Y],
    cx: ToGenerator[C, X],
    dw: ToGenerator[D, W],
    ev: ToGenerator[E, V],
    fu: ToGenerator[F, U],
    gt: ToGenerator[G, T]
  ): ToGenerator[(A, B, C, D, E, F, G), (Z, Y, X, W, V, U, T)] =
    new ToGenerator[(A, B, C, D, E, F, G), (Z, Y, X, W, V, U, T)] {
      def apply(t: (A, B, C, D, E, F, G)) = az(t._1)
        .zip(by(t._2))
        .zip(cx(t._3))
        .zip(dw(t._4))
        .zip(ev(t._5))
        .zip(fu(t._6))
        .zip(gt(t._7))
        .map { case ((((((z, y), x), w), v), u), t) =>
          (z, y, x, w, v, u, t)
        }
    }

  implicit def zip8toGen[A, B, C, D, E, F, G, H, Z, Y, X, W, V, U, T, S](
    implicit az: ToGenerator[A, Z],
    by: ToGenerator[B, Y],
    cx: ToGenerator[C, X],
    dw: ToGenerator[D, W],
    ev: ToGenerator[E, V],
    fu: ToGenerator[F, U],
    gt: ToGenerator[G, T],
    hs: ToGenerator[H, S]
  ): ToGenerator[(A, B, C, D, E, F, G, H), (Z, Y, X, W, V, U, T, S)] =
    new ToGenerator[(A, B, C, D, E, F, G, H), (Z, Y, X, W, V, U, T, S)] {
      def apply(t: (A, B, C, D, E, F, G, H)) = az(t._1)
        .zip(by(t._2))
        .zip(cx(t._3))
        .zip(dw(t._4))
        .zip(ev(t._5))
        .zip(fu(t._6))
        .zip(gt(t._7))
        .zip(hs(t._8))
        .map { case (((((((z, y), x), w), v), u), t), s) =>
          (z, y, x, w, v, u, t, s)
        }
    }

  @JsonCodec
  case class Ratings(
    globalBias: Double,
    noLiveJudgeAdjustment: Double,
    noOpponentAdjustment: Double,
    questionEase: Vector[(QuestionId, Double)],
    honestSkills: Map[String, Double],
    dishonestSkills: Map[String, Double],
    judgeLeadingSkills: Map[String, Double],
    judgeJudgingSkills: Map[String, Double]
  ) {
    def averageSkills = List(honestSkills, dishonestSkills, judgeLeadingSkills, judgeJudgingSkills)
      .combineAll
      .mapVals(_ / 4)
  }
  object Ratings {
    def empty = Ratings(0.0, 0.0, 0.0, Vector(), Map(), Map(), Map(), Map())
  }

  def computeRatings(
    debates: Vector[Debate],
    debaterSet: Set[String],
    timeCutoff: Option[Long] = None
  ): Ratings = {
    val questions = debates.map(QuestionId.fromDebate)
    val debaters  = debaterSet.toVector

    // val data         = loadDebateData.unsafeRunSync()
    val globalBias            = Normal(0, 1).latent
    val noOpponentAdjustment  = Normal(0, 1).latent
    val noLiveJudgeAdjustment = Normal(0, 1).latent
    val questionEase          = Normal(0, 1).latentVec(questions.size)
    val honestSkill           = Normal(0, 1).latentVec(debaters.size)
    val dishonestSkill        = Normal(0, 1).latentVec(debaters.size)
    val judgeLeadingSkill     = Normal(0, 1).latentVec(debaters.size)
    val judgeJudgingSkill     = Normal(0, 1).latentVec(debaters.size)
    // val debateVariance = Gamma(0.5, 100).latent

    val personIndex   = debaters.zipWithIndex.toMap
    val questionIndex = questions.zipWithIndex.toMap

    val fullModelOpt = {
      case class DebateResult(
        debate: Debate,
        judge: String,
        judgment: Vector[Double],
        numDebateRounds: Int
      ) {
        val probCorrect  = judgment(debate.setup.correctAnswerIndex)
        def logitCorrect = math.log(probCorrect / (1 - probCorrect)) / math.log(2.0)
        val adjustedProbCorrect = math.pow(
          2,
          debate
            .setup
            .rules
            .scoringFunction
            .eval(numDebateRounds, judgment, debate.setup.correctAnswerIndex)
        )
        val adjustedLogitCorrect =
          math.log(adjustedProbCorrect / (1 - adjustedProbCorrect)) / math.log(2.0)
      }

      val results: Vector[DebateResult] = debates
        .flatMap(d => d.result.map(d -> _))
        .filter { case (_, r) =>
          timeCutoff.forall(r.timestamp <= _)
        }
        .flatMap { case (debate, result) =>
          val liveResultOpt = result
            .judgingInfo
            .map(info =>
              DebateResult(
                debate,
                debate.setup.roles.get(Judge).get,
                info.finalJudgement,
                debate.numDebateRounds
              )
            )
          val offlineResults = debate
            .offlineJudgingResults
            .toList
            .map { case (judge, judgment) =>
              judgment
                .result
                .map { result =>
                  val numContinues =
                    judgment.mode match {
                      case OfflineJudgingMode.Timed =>
                        debate.numDebateRounds
                      case OfflineJudgingMode.Stepped =>
                        judgment.judgments.size
                    }
                  DebateResult(debate, judge, result.distribution, numContinues)
                }
            }
          (liveResultOpt :: offlineResults).flatten
        }

      // adjust the logit? Tempted not to.
      val resultLogits = results.map(_.logitCorrect)
      val resultModels = results.map { case DebateResult(debate, judge, _, _) =>
        val setup                           = debate.setup
        val roles                           = setup.roles
        def add(x: Real): State[Real, Unit] = State.modify(_ + x)
        def addOpt(xOpt: Option[Real]): State[Real, Unit] =
          xOpt.fold(State.pure[Real, Unit](()))(x => State.modify(_ + x))
        val makeLogit =
          for {
            _ <- add(questionEase(questionIndex(QuestionId.fromDebate(debate))))
            _ <- addOpt(
              roles
                .get(Debater(setup.correctAnswerIndex))
                .map(debater => honestSkill(personIndex(debater)))
            )
            _ <- addOpt(
              roles
                .get(Debater(1 - setup.correctAnswerIndex))
                .map(debater => -dishonestSkill(personIndex(debater)))
            )
            _ <- addOpt(
              Option(noOpponentAdjustment).filter(_ => roles.keySet.filter(_.isDebater).size == 1)
            )
            _ <- add(
              roles
                .get(Judge)
                .fold(noLiveJudgeAdjustment)(judge => judgeLeadingSkill(personIndex(judge)))
            )
            _     <- add(judgeJudgingSkill(personIndex(judge)))
            logit <- State.get[Real]
          } yield logit

        makeLogit.runS(globalBias).value
      }

      if (resultLogits.isEmpty)
        None
      else
        Some(Model.observe(resultLogits, Vec.from(resultModels).map(Normal(_, 1.0))))
    }

    val allVariablesModel = Model.track(
      Set(globalBias, noLiveJudgeAdjustment, noOpponentAdjustment) ++ questionEase.toVector ++
        honestSkill.toVector ++ dishonestSkill.toVector ++ judgeLeadingSkill.toVector ++
        judgeJudgingSkill.toVector
    )

    if (fullModelOpt.isEmpty)
      return Ratings.empty
    else {

      val fullModel = (List(fullModelOpt).flatten ++ List(allVariablesModel)).reduce(_ merge _)

      val mapEstimate = fullModel.optimize(
        (
          globalBias,
          noLiveJudgeAdjustment,
          noOpponentAdjustment,
          questionEase,
          honestSkill,
          dishonestSkill,
          judgeLeadingSkill,
          judgeJudgingSkill
        )
      )

      Ratings(
        globalBias = mapEstimate._1,
        noLiveJudgeAdjustment = mapEstimate._2,
        noOpponentAdjustment = mapEstimate._3,
        questionEase = questions.zip(mapEstimate._4),
        honestSkills = debaters.zip(mapEstimate._5).toMap,
        dishonestSkills = debaters.zip(mapEstimate._6).toMap,
        judgeLeadingSkills = debaters.zip(mapEstimate._7).toMap,
        judgeJudgingSkills = debaters.zip(mapEstimate._8).toMap
      )
    }

  }
}
