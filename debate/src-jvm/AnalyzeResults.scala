package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import io.circe.generic.JsonCodec
import debate.quality.QuALITYStory
import debate.JudgingResult
import cats.Monoid

import jjm.ling.Text
import jjm.metrics._
import jjm.implicits._

import shapeless._
import shapeless.syntax.singleton._
import shapeless.record._
import jjm.ling.ESpan

@JsonCodec
case class Quartiles2(
  min: Double,
  firstQuartile: Double,
  median: Double,
  thirdQuartile: Double,
  max: Double
) {
  def getMetrics: MapTree[String, Metric] = MapTree.fromPairs(
    "min"            -> Metric.double(min),
    "first quartile" -> Metric.double(firstQuartile),
    "median"         -> Metric.double(median),
    "third quartile" -> Metric.double(thirdQuartile),
    "max"            -> Metric.double(max)
  )
}
object Quartiles2 {
  def fromValues(values: Vector[Double]) = {
    val sortedValues = values.sorted
    def get(frac: Double) = sortedValues
      .lift(math.max(0, (frac * values.size).toInt - 1))
      .getOrElse(Double.NaN)
    Quartiles2(get(0.0), get(0.25), get(0.5), get(0.75), get(1.00))
  }
  implicit val quartiles2HasMetrics: HasMetrics[Quartiles2] =
    new HasMetrics[Quartiles2] {
      def getMetrics(q: Quartiles2) = q.getMetrics
    }
}

case class Numbers2[N](values: Vector[N])(implicit N: Numeric[N]) {
  def stats = {
    val dValues = values.map(N.toDouble)
    val sum     = dValues.sum
    val mean    = sum / values.size
    Numbers2.Stats(
      values.size,
      sum,
      mean,
      dValues.iterator.map(x => math.pow(x - mean, 2)).sum / values.size,
      Quartiles2.fromValues(dValues)
    )
  }
}
object Numbers2 {
  def apply[N: Numeric](n: N): Numbers2[N] = Numbers2(Vector(n))
  case class Stats(count: Int, sum: Double, mean: Double, variance: Double, quartiles: Quartiles2) {
    def stdev = math.sqrt(variance)
    def getMetrics: MapTree[String, Metric] = MapTree.fork(
      "count"     -> MapTree.leaf[String](Metric.int(count)),
      "sum"       -> MapTree.leaf[String](Metric.double(sum)),
      "mean"      -> MapTree.leaf[String](Metric.double(mean)),
      "variance"  -> MapTree.leaf[String](Metric.double(variance)),
      "stdev"     -> MapTree.leaf[String](Metric.double(stdev)),
      "quartiles" -> quartiles.getMetrics
    )
  }

  implicit def numbers2Monoid[A: Numeric]: Monoid[Numbers2[A]] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }
  implicit def numbers2HasMetrics[A] =
    new HasMetrics[Numbers2[A]] {
      def getMetrics(nums: Numbers2[A]) = nums.stats.getMetrics
    }
}

case class Confidences(data: Vector[Confidences.Prediction] = Vector()) {
  import Confidences.Prediction
  def stats(numBins: Int) = Confidences.Stats(
    (0 until numBins)
      .map { binNum =>
        val binMin = binNum / numBins.toDouble
        val binMax = (binNum + 1) / numBins.toDouble
        (binMin, binMax) -> {
          val bin = data.filter { case Prediction(confidence, _) =>
            (confidence >= binMin && confidence < binMax) || binMax == 1.0 && confidence == 1.0
          }
          Confidences.BinStats(
            numCorrect = bin.foldMap(_.correctness),
            confidences = Numbers(bin.map(_.confidence))
          )
        }
      }
      .toMap
  )
}
object Confidences {
  case class Prediction(confidence: Double, correctness: Double)
  case class BinStats(numCorrect: Double, confidences: Numbers[Double]) {
    def numPredictions = confidences.values.size
    def accuracy       = numCorrect / numPredictions
    def confidence     = confidences.stats.mean
  }
  object BinStats {
    implicit val confidencesBinStatsMonoid: Monoid[BinStats] = {
      import cats.derived.auto.monoid._
      cats.derived.semiauto.monoid
    }
  }

  def apply(ds: Prediction*): Confidences = Confidences(ds.toVector)
  case class Stats(bins: Map[(Double, Double), BinStats]) {
    def accuracy = bins.values.toList.combineAll.accuracy
    def ece =
      bins
        .toList
        .filter(_._2.numPredictions > 0)
        .foldMap { case (_, acc) =>
          WeightedNumbers(math.abs(acc.accuracy - acc.confidence), weight = acc.numPredictions)
        }
        .stats
        .weightedMean

    def getTree: MapTree[String, Metric] = MapTree.fork(
      "accuracy" -> MapTree.leaf(Metric.double(accuracy)),
      "calibration" ->
        MapTree.fork(
          bins.map { case ((lb, ub), stats) =>
            s"$lb-$ub" ->
              MapTree.fromPairs(
                "accuracy"        -> Metric.double(stats.accuracy),
                "num predictions" -> Metric.int(stats.numPredictions)
              )
          }
        ),
      "expected calibration error" -> MapTree.leaf(Metric.double(ece))
    )
  }
  object Stats {
    implicit val confidencesStatsHasMetrics =
      new HasMetrics[Confidences.Stats] {
        def getMetrics(conf: Confidences.Stats) = conf.getTree
      }
  }
  implicit val confidencesMonoid: Monoid[Confidences] = {
    import cats.derived.auto.monoid._
    cats.derived.semiauto.monoid
  }
  implicit val confidencesHasMetrics =
    new HasMetrics[Confidences] {
      def getMetrics(conf: Confidences) = conf.stats(10).getTree
    }
}

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object AnalyzeResults
    extends CommandIOApp(
      name = "mill -i debate.jvm.runMain debate.AnalyzeResults",
      header = "Do some data analysis and checks for the paper."
    ) {

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "directory path",
      help = "Directory from which to read the debates."
    )
    .withDefault(Paths.get("save"))

//   val outO = Opts
//     .option[NIOPath](
//       "out",
//       metavar = "file path",
//       help = "file in which to save the LM-readable debates."
//     )
//     .withDefault(Paths.get("debates-readable.jsonl"))

//   val roomNameOptO =
//     Opts
//       .option[String](
//         "room",
//         metavar = "room name",
//         help = "Room name to print out the debate for."
//       )
//       .orNone

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = saveO.map { save =>
    Blocker[IO].use { blocker =>
      for {
        server <- Server.create(Paths.get("data"), save, Nil, blocker)
        rooms  <- server.officialDebates.rooms.get
        _ <- analyzeDebates(
          rooms.toList.map(p => p._1 -> p._2.debate.debate),
          server.qualityDataset
        )
      } yield ExitCode.Success
    }
  }

  case class AnalyzedOnlineDebate(
    roomName: String,
    debate: Debate,
    endTime: Long,
    result: JudgingResult,
    questionDifficultyOpt: Option[Int]
  ) {
    def probabilityCorrect     = result.finalJudgement(debate.setup.correctAnswerIndex)
    val setting: DebateSetting = DebateSetting.fromDebate(debate)
    def original               = AnalyzedDebate(roomName, debate, questionDifficultyOpt)
  }

  case class AnalyzedDebate(roomName: String, debate: Debate, questionDifficultyOpt: Option[Int]) {
    def isNotTooLittleContextRequired = questionDifficultyOpt.forall(_ > 1)
    val onlineJudgment                = debate.result.flatMap(_.judgingInfo)
    val isOnline               = onlineJudgment.exists(_ => debate.setup.roles.contains(Judge))
    val setting: DebateSetting = DebateSetting.fromDebate(debate)
    def filtered = debate
      .result
      .flatMap(result =>
        result
          .judgingInfo
          .map(judgment =>
            AnalyzedOnlineDebate(
              roomName,
              debate,
              result.timestamp,
              judgment,
              questionDifficultyOpt
            )
          )
          .filter(_ => isOnline && isNotTooLittleContextRequired)
      )
  }

  def includeIf[A](a: A, include: Boolean) =
    if (include)
      Proportion.included(a)
    else
      Proportion.excluded(a)
  def correctIf[A](a: A, correct: Boolean) =
    if (correct)
      Accuracy.correct(a)
    else
      Accuracy.incorrect(a)

  implicit val stringsHaveMetrics: HasMetrics[Vector[String]] =
    new HasMetrics[Vector[String]] {
      def getMetrics(xs: Vector[String]): MapTree[String, Metric] = MapTree
        .leaf[String](Metric.metadata(xs.mkString(", ")))
    }

  val sortSpec = {
    import Metric._
    import MapTree.SortQuery._
    val double =
      (mv: Metric) =>
        mv match {
          case MetricMetadata(_) =>
            0.0
          case MetricBool(x) =>
            if (x)
              1.0
            else
              0.0
          case MetricInt(x) =>
            x.toDouble
          case MetricDouble(x) =>
            x
          case MetricIntOfTotal(x, _) =>
            x.toDouble
        }
    val inc = value[String](double)
    // val dec = value[String](double.andThen(_ * -1))
    // TODO: change to make sense for this app
    List("full question" :: "f1" :: inc, "full question" :: "acc-lb" :: inc, "num predicted" :: inc)

  }

  def getMetricsString[M: HasMetrics](m: M) = m
    .getMetrics
    .toStringPrettySorted(identity, x => x.render, sortSpec)

  def getArticleId(d: Debate): String = SourceMaterial
    .quality
    .getOption(d.setup.sourceMaterial)
    .map(_.articleId)
    .getOrElse("N/A")

  def getQuestionSplit(d: Debate, quality: Map[String, QuALITYStory]): String = SourceMaterial
    .quality
    .getOption(d.setup.sourceMaterial)
    .map(_.articleId)
    .flatMap(quality.get)
    .flatMap(_.questions.find(_._2.question == d.setup.question))
    .map(_._2.split)
    .getOrElse("N/A")

  def getQuestionId(debate: Debate) = s"${getArticleId(debate)}: ${debate.setup.question}"

  def getConsultancySetting(d: AnalyzedDebate) = {
    val debaterIndices = d
      .debate
      .setup
      .roles
      .keySet
      .collect { case Debater(i) =>
        i
      }
    if (debaterIndices.size > 1) {
      None
    } else {
      Some(debaterIndices.head == d.debate.setup.correctAnswerIndex)
    }
  }

  def isConsultantHonest(d: AnalyzedOnlineDebate) = {
    val setting = getConsultancySetting(d.original)
    require(setting.nonEmpty)
    setting.get
  }

  def analyzeVariance(debates: List[AnalyzedOnlineDebate]) = {
    def varianceEstimate[N](nums: Numbers[N])(implicit N: Numeric[N]) = {
      val mean = nums.stats.mean
      nums.values.iterator.map(x => math.pow(N.toDouble(x) - mean, 2)).sum / (nums.values.size - 1)
    }

    val varianceMetricsRaw = debates.foldMap(d =>
      Chosen(
        Map(
          d.setting ->
            (
              "1. aggregate accuracy variance" ->> Numbers(math.round(d.probabilityCorrect)) ::
                "2. per-story accuracy variance" ->>
                Chosen(Map(getArticleId(d.debate) -> Numbers(math.round(d.probabilityCorrect)))) ::
                "2.5. debates per story" ->> Count(getArticleId(d.debate)) ::
                "3. per-question accuracy variance" ->>
                Chosen(
                  Map(
                    s"${getArticleId(d.debate)}: ${d.debate.setup.question}" ->
                      Numbers(math.round(d.probabilityCorrect))
                  )
                ) ::
                "3.5. debates per question" ->>
                Count(getArticleId(d.debate) -> d.debate.setup.question) :: HNil
            )
        )
      )
    )
    val varianceMetrics = varianceMetricsRaw.map(
      _.updateWith("1. aggregate accuracy variance")(varianceEstimate(_))
        .updateWith("2. per-story accuracy variance")(
          _.data
            .values
            .toList
            .filter(_.values.size > 1)
            .foldMap(nums => Numbers(varianceEstimate(nums)))
            .stats
            .mean
        )
        .updateWith("3. per-question accuracy variance") { x =>
          val nums = x
            .data
            .values
            .toList
            .filter(_.values.size > 1)
            .foldMap(nums => Numbers(varianceEstimate(nums)))
          if (nums.values.isEmpty) {
            Double.NaN
          } else
            nums.stats.mean
        }
    )

    println("===== Variance metrics =====")
    println(getMetricsString(varianceMetrics))

    println("===== Question/debate count metrics =====")
    val questionCountMetrics = debates
      .groupBy(d => d.setting -> getConsultancySetting(d.original) -> getQuestionId(d.debate))
      .values
      .toList
      .map(_.size)
      .foldMap(Counts(_))
    println(questionCountMetrics.histogramString(40))
    println("===== Offline judgment count metrics =====")
    val offlineCountMetrics = debates
      .map(_.debate.offlineJudgingResults.values.flatMap(_.result).size + 1)
      .foldMap(Counts(_))
    println(offlineCountMetrics.histogramString(40))

    println("===== Info-theoretic metrics =====")

    def getMutualInformationBits[I, A](conf: Confusion[I, A]) = {
      val total   = conf.matrix.values.toList.foldMap(_.values.toList.foldMap(_.size))
      val xTotals = conf.matrix.mapVals(_.values.toList.foldMap(_.size.toDouble))
      val yTotals = conf.matrix.values.toList.combineAll.mapVals(_.size.toDouble)
      // val px = rowTotals.map(_.toDouble / total)
      // val py = colTotals.map(_.toDouble / total)

      val mi =
        conf
          .matrix
          .toList
          .foldMap { case (x, ys) =>
            ys.toList
              .foldMap { case (y, items) =>
                val count = items.size.toDouble
                count *
                  math
                    .log(count * total / (xTotals.getOrElse(x, 0.0) * yTotals.getOrElse(y, 0.0))) /
                  math.log(2.0)
              }
          } / total
      mi
    }
    val debateSelfConf = debates
      .foldMap { d =>
        val c = d.probabilityCorrect > 0.5
        Chosen(
          Map(
            (d.setting -> getConsultancySetting(d.original)) ->
              Confusion(Map(c -> Map(c -> List(()))))
          )
        )
      }
      .map(getMutualInformationBits)
    println(s"Self-information: ${getMetricsString(debateSelfConf)}")

    def getConfs[I](setting: I, corrects: List[Boolean]): Chosen[I, Confusion[Boolean, Unit]] =
      corrects.headOption match {
        case Some(c1) =>
          corrects
            .tail
            .foldMap(c2 => Chosen(Map(setting -> Confusion(Map(c1 -> Map(c2 -> List(()))))))) |+|
            getConfs(setting, corrects.tail)
        case None =>
          Chosen(Map())
      }

    // how debates vary on the same question
    val sameQuestionConfMatrix = debates
      .groupBy(d => (d.setting -> getConsultancySetting(d.original)) -> getQuestionId(d.debate))
      .toVector
      .foldMap { case ((setting, _), debatesForQuestion) =>
        getConfs(setting, debatesForQuestion.map(_.probabilityCorrect > 0.5))
      }
    println("===== Confusion matrices for same question debates =====")
    println(
      getMetricsString(
        sameQuestionConfMatrix.map(x => Vector(x.stats.prettyString(classFreqBound = 0)))
      )
    )
    println(
      s"Mutual information (w/o replacement): ${getMetricsString(sameQuestionConfMatrix.map(getMutualInformationBits))}"
    )

    val sameDebateConfMatrix = debates.foldMap { d =>
      val liveJudgeWasCorrect = d.probabilityCorrect > 0.5
      d.debate
        .offlineJudgingResults
        .values
        .toList
        .flatMap(_.result)
        .map(_.distribution(d.debate.setup.correctAnswerIndex) > 0.5)
        .foldMap(offlineJudgeWasCorrect =>
          Chosen(
            Map(
              (d.setting -> getConsultancySetting(d.original)) ->
                Confusion(Map(liveJudgeWasCorrect -> Map(offlineJudgeWasCorrect -> List(()))))
            )
          )
        )
    }
    println("===== Confusion matrices for same debate judgments =====")
    println(
      getMetricsString(
        sameDebateConfMatrix.map(x => Vector(x.stats.prettyString(classFreqBound = 0)))
      )
    )
    println(
      s"Mutual information (w/o replacement): ${getMetricsString(sameDebateConfMatrix.map(getMutualInformationBits))}"
    )

    def binaryPoE(x: Double, y: Double) = {
      val unnormPTrue  = x * y
      val unnormPFalse = (1 - x) * (1 - y)
      unnormPTrue / (unnormPTrue + unnormPFalse)
    }

    def offlineEnsembledAccuracy(numOfflineJudges: Int) = debates.foldMap { d =>
      // val liveJudgeWasCorrect = d.probabilityCorrect > 0.5
      d.debate
        .offlineJudgingResults
        .values
        .toList
        .flatMap(_.result)
        .grouped(numOfflineJudges)
        .filter(_.size == numOfflineJudges)
        .toList
        .map(_.map(_.distribution(d.debate.setup.correctAnswerIndex)))
        .foldMap(offlinePCorrects =>
          Chosen(
            Map(
              (d.setting -> getConsultancySetting(d.original)) ->
                correctIf((), offlinePCorrects.foldLeft(d.probabilityCorrect)(binaryPoE) > 0.5)
            )
          )
        )
    }
    println("===== Offline judge accuracy PoE ensemble accuracies =====")
    println(
      getMetricsString(
        "1 offline judge" ->> offlineEnsembledAccuracy(1) ::
          "2 offline judges" ->> offlineEnsembledAccuracy(2) ::
          "3 offline judges" ->> offlineEnsembledAccuracy(3) :: HNil
      )
    )

  }

  def analyzePerformance(debates: List[AnalyzedOnlineDebate]) = {
    def accAtThreshold(d: AnalyzedOnlineDebate, threshold: Double) = {
      val p       = d.probabilityCorrect
      val correct = p > 0.5
      if (math.max(p, 1.0 - p) >= threshold)
        correctIf(d, correct)
      else
        Accuracy[AnalyzedOnlineDebate]()
    }
    val metrics = debates.foldMap(d =>
      Chosen(
        Map(
          d.setting ->
            (
              "accuracy" ->> correctIf(d, d.probabilityCorrect > 0.5) ::
                "length" ->> Numbers(d.debate.numDebateRounds) ::
                "reward" ->> Numbers(d.result.judgeReward) ::
                "confidence" ->>
                Numbers(math.max(d.probabilityCorrect, 1 - d.probabilityCorrect)) ::
                "correct speaker" ->> FewClassCount(d.result.correctAnswerIndex) ::
                "winning speaker" ->>
                FewClassCount(d.result.finalJudgement.zipWithIndex.maxBy(_._1)._2) ::
                "accuracy at confidence thresholds" ->>
                (".60" ->> accAtThreshold(d, 0.6) :: ".70" ->> accAtThreshold(d, 0.7) ::
                  ".80" ->> accAtThreshold(d, 0.8) :: ".90" ->> accAtThreshold(d, 0.9) ::
                  ".95" ->> accAtThreshold(d, 0.95) :: ".99" ->> accAtThreshold(d, 0.99) :: HNil) ::
                "calibration (final judgment)" ->>
                Confidences(
                  Confidences.Prediction(
                    confidence = math.max(d.probabilityCorrect, 1 - d.probabilityCorrect),
                    correctness =
                      if (d.probabilityCorrect > 0.5)
                        1.0
                      else if (d.probabilityCorrect < 0.5)
                        0.0
                      else
                        0.5
                  )
                ) ::
                "calibration (all turns)" ->>
                d.debate
                  .rounds
                  .collect { case JudgeFeedback(dist, _, _) =>
                    dist
                  }
                  .foldMap { dist =>
                    val probabilityCorrect = dist(d.debate.setup.correctAnswerIndex)
                    Confidences(
                      Confidences.Prediction(
                        confidence = math.max(probabilityCorrect, 1 - probabilityCorrect),
                        correctness =
                          if (probabilityCorrect > 0.5)
                            1.0
                          else if (probabilityCorrect < 0.5)
                            0.0
                          else
                            0.5
                      )
                    )
                  } :: HNil
            )
        )
      )
    )
    println("===== Performance metrics =====")
    println(getMetricsString(metrics))
  }

  implicit class RichChosen[K, V](chosen: Chosen[K, V]) {
    def mapKeys[K2](f: K => K2)(implicit M: Monoid[V]): Chosen[K2, V] = Chosen(
      chosen
        .data
        .toList
        .foldMap { case (k, v) =>
          Map(f(k) -> v)
        }
    )
  }

  import java.time.Instant

  def getDebatesFilteredForTime(
    allDebates: List[(String, Debate)],
    quality: Map[String, QuALITYStory]
  ) = allDebates
    .flatMap { case (roomName, debate) =>
      debate
        .result
        .filter(res =>
          Instant
            .ofEpochMilli(debate.setup.creationTime)
            .isAfter(java.time.Instant.parse("2023-02-10T00:00:00Z")) &&
            Instant
              .ofEpochMilli(res.timestamp)
              .isBefore(java.time.Instant.parse("2023-09-01T00:00:00Z"))
        )
        .as(roomName -> debate)
    }
    .map { case (roomName, debate) =>
      val questionDifficultyOpt =
        for {
          source      <- SourceMaterial.quality.getOption(debate.setup.sourceMaterial)
          story       <- quality.get(source.articleId)
          question    <- story.questions.values.toList.find(_.question == debate.setup.question)
          annotations <- question.annotations
          score       <- annotations.context.meanOpt.map(math.round(_))
        } yield score.toInt

      AnalyzedDebate(roomName, debate, questionDifficultyOpt)
    }

  def balanceDebates(unbalancedDebates: List[AnalyzedDebate]) =
    unbalancedDebates
      .flatMap { d =>
        val judges =
          d.debate.offlineJudgingResults.filter(_._2.result.nonEmpty).keySet ++
            d.debate.setup.roles.get(Judge)
        judges.toList.map(j => (j -> getArticleId(d.debate)) -> d)
      }
      .groupByNel(_._1) // group by (judge, article)
      .values
      .map(
        _.minimumBy(_._2.debate.result.map(_.timestamp).getOrElse(Long.MaxValue))
      ) // only include the first judgment that each judge made on each article
      .filter(p =>
        p._2.debate.setup.roles.get(Judge).exists(_ == p._1._1)
      ) // only include the debate if this first judgment was done as an online judge
      .map(_._2)
      .toList
      .flatMap(_.filtered) // do debate-specific filtering: is over, is online, is not too easy
      .groupBy(_.setting)
      .transform { case (setting, _debates) =>
        if (setting.isDebate)
          _debates
        else {
          val debates = _debates
          // .groupByNel(d => getQuestionId(d.debate) -> isConsultantHonest(d))
          // .mapVals(_.minimumBy(_.endTime))
          // .values
          // .toList
          // balance consultancies
          val numHonest     = debates.count(isConsultantHonest)
          val numDishonest  = debates.size - numHonest
          val difference    = math.abs(numHonest - numDishonest)
          val hasMoreHonest = numHonest > numDishonest

          // deterministically (randomly) shuffle debates
          val debatesByQuestion = debates.groupBy(d => getQuestionId(d.debate))
          val rand              = new scala.util.Random(876)
          // val rand = new scala.util.Random(10971420)
          val commonDebates = rand.shuffle(
            debatesByQuestion
              .filter(p =>
                p._2.exists(isConsultantHonest) && p._2.exists(d => !isConsultantHonest(d))
              )
              .toList
              .sortBy(_._1)
              .map(p =>
                p.copy(_2 = rand.shuffle(p._2))
              ) // randomize order of consultancies for each question
          )

          // first, balance by removing consultancies from questions that have both honest and dishonest consultancies
          // this will get us to end up with a larger number of total questions covered in the data,
          // which makes it closer to i.i.d.
          val (filteredCommonDebates, remainingDifference) =
            commonDebates.foldLeft((Vector.empty[AnalyzedOnlineDebate], difference)) {
              case ((acc, remainingDifference), (_, debates)) =>
                if (remainingDifference <= 0)
                  (acc ++ debates, 0)
                else {
                  val (honest, dishonest) = debates.partition(isConsultantHonest)
                  val (more, less) =
                    if (hasMoreHonest)
                      (honest, dishonest)
                    else
                      (dishonest, honest)

                  val numMoreToRemove = math.min(remainingDifference, more.size)
                  val debatesToAdd    = more.drop(numMoreToRemove) ++ less
                  (acc ++ debatesToAdd, remainingDifference - numMoreToRemove)
                }
            }
          require(remainingDifference >= 0)
          val honest = rand.shuffle(debatesByQuestion.filter(_._2.forall(isConsultantHonest)))
          val dishonest = rand
            .shuffle(debatesByQuestion.filter(_._2.forall(d => !isConsultantHonest(d))))
          val (more, less) =
            if (hasMoreHonest)
              (honest, dishonest)
            else
              (dishonest, honest)

          require(remainingDifference < more.size)
          // now, if it's still imbalanced, filter from the remainder of questions
          // which only have honest or dishonest consultancies
          val balancedUniques =
            more.drop(remainingDifference).values.flatten.toList ++ less.values.flatten.toList

          filteredCommonDebates ++ balancedUniques
        }
      }
      .values
      .flatten
      .toList

  def analyzeDebates(
    allDebates: List[(String, Debate)],
    quality: Map[String, QuALITYStory]
  ): IO[Unit] = IO {

    val unbalancedDebates = getDebatesFilteredForTime(allDebates, quality)
    val debates           = balanceDebates(unbalancedDebates)

    def printInitialMetrics(debates: List[AnalyzedDebate]) = {
      val initialMetrics = debates.foldMap { debate =>
        Chosen(
          Map(
            debate.setting ->
              ("data split" ->> FewClassCount(getQuestionSplit(debate.debate, quality)) ::
                "online" ->> includeIf(debate, debate.isOnline) ::
                "not too easy" ->> includeIf(debate, debate.isNotTooLittleContextRequired) ::
                "honest consultancies" ->> FewClassCount(getConsultancySetting(debate)) ::
                "rounded difficulties" ->> FewClassCount(debate.questionDifficultyOpt.get) ::
                "included in final analysis" ->>
                includeIf(debate, debate.isOnline && debate.isNotTooLittleContextRequired) :: HNil)
          )
        )
      }

      println("----- Metrics by setting -----")
      println(getMetricsString(initialMetrics))

      println("----- Aggregate metrics -----")
      println(getMetricsString(initialMetrics.combineAll))
    }

    println("===== Initial metrics (unbalanced) =====")
    printInitialMetrics(unbalancedDebates)
    println("===== Initial metrics (balanced) =====")
    printInitialMetrics(debates.map(_.original))

    analyzeVariance(debates)

    analyzePerformance(debates)

    // println("Longest debates:")
    // println(
    //   getMetricsString(
    //     debates
    //       .foldMap(d => Chosen(Map(d.setting -> Vector(d))))
    //       .map(
    //         _.sortBy(-_.debate.numDebateRounds)
    //           .take(5)
    //           .map(d => s"${d.roomName} (${d.debate.numDebateRounds})")
    //           .toVector
    //       )
    //   )
    // )

    println("Highest-confidence turn-0 debates:")
    println(
      getMetricsString(
        debates
          .foldMap(d =>
            Chosen(
              Map(
                d.debate
                  .rounds
                  .collect { case JudgeFeedback(dist, _, _) =>
                    dist(d.debate.setup.correctAnswerIndex)
                  }
                  .head -> Vector(d.roomName)
              )
            )
          )
          .map(_.take(5))
      )
    )

    println("Rounded/binned confidences for turn 0:")
    println(
      getMetricsString(
        debates
          .foldMap { d =>
            val turn0Confidence =
              d.debate
                .rounds
                .collect { case JudgeFeedback(dist, _, _) =>
                  dist(d.debate.setup.correctAnswerIndex)
                }
                .head
            val bin = math.round(turn0Confidence * 10) / 10.0
            FewClassCount(f"$bin%.2f")
          }
          .map(_.take(5))
      )
    )

    println("Story lengths:")
    println(
      getMetricsString(
        "Story #tokens (per story)" ->>
          debates
            .map(_.debate.setup.sourceMaterial)
            .toSet
            .toList
            .foldMap(sourceMaterial => Numbers(sourceMaterial.contents.size)) ::
          "Story #chars (per story)" ->>
          debates
            .map(_.debate.setup.sourceMaterial)
            .toSet
            .toList
            .foldMap(sourceMaterial => Numbers(Text.render(sourceMaterial.contents).size)) ::
          "Story #tokens (per debate)" ->>
          debates.foldMap(d => Numbers(d.debate.setup.sourceMaterial.contents.size)) ::
          "Story #chars (per debate)" ->>
          debates.foldMap(d => Numbers(Text.render(d.debate.setup.sourceMaterial.contents).size)) ::
          HNil
      )
    )

    // println("Work contributions:")
    // val workContributions = debates
    //   .foldMap(d => d.setting -> DebateStats.fromDebate(d.debate))
    //   .mapKeys(_.toString)
    //   .map(_.map(_.wins.total))
    // println(getMetricsString(workContributions))
    // println(getMetricsString(workContributions.map(_.data.values.toList.foldMap(Numbers(_)))))

    println("Data statistics")
    println(
      getMetricsString(
        debates
          .foldMap { d =>
            val story = d.debate.setup.sourceMaterial.contents
            Chosen(
              Map(
                d.setting ->
                  ("debater chars per round" ->>
                    d.debate
                      .rounds
                      .collect {
                        case SimultaneousSpeeches(speeches) =>
                          Numbers(
                            speeches.unorderedFoldMap(speech =>
                              SpeechSegments.getSpeechLength(story, speech.content)
                            )
                          )
                        case SequentialSpeeches(speeches) =>
                          Numbers(
                            speeches.unorderedFoldMap(speech =>
                              SpeechSegments.getSpeechLength(story, speech.content)
                            )
                          )
                      }
                      .combineAll ::
                    // "total quoted tokens per debate" ->>
                    // Numbers(
                    //   d.debate
                    //     .rounds
                    //     .collect {
                    //       case SimultaneousSpeeches(speeches) =>
                    //         speeches
                    //       case SequentialSpeeches(speeches) =>
                    //         speeches
                    //     }
                    //     .foldMap { speeches =>
                    //       speeches
                    //         .values
                    //         .toVector
                    //         .foldMap(speech =>
                    //           speech
                    //             .content
                    //             .foldMap {
                    //               case SpeechSegment.Quote(span) =>
                    //                 (span.begin until span.endExclusive).toSet
                    //               case _ =>
                    //                 Set[Int]()
                    //             }
                    //         )
                    //     }
                    //     .size
                    // ) ::
                    // "new quoted tokens per round" ->>
                    // d.debate
                    //   .rounds
                    //   .collect {
                    //     case SimultaneousSpeeches(speeches) =>
                    //       speeches
                    //     case SequentialSpeeches(speeches) =>
                    //       speeches
                    //   }
                    //   .foldLeft(Set.empty[Int] -> Numbers[Int](Vector())) {
                    //     case ((coveredTokens, prevData), speeches) =>
                    //       val newTokens =
                    //         speeches
                    //           .values
                    //           .toVector
                    //           .foldMap(speech =>
                    //             speech
                    //               .content
                    //               .foldMap {
                    //                 case SpeechSegment.Quote(span) =>
                    //                   (span.begin until span.endExclusive).toSet
                    //                 case _ =>
                    //                   Set[Int]()
                    //               }
                    //           ) -- coveredTokens

                    //       (coveredTokens ++ newTokens, prevData |+| Numbers(newTokens.size.toInt))
                    //   }
                    //   ._2 ::
                    "all quoted chars per round" ->>
                    d.debate
                      .rounds
                      .collect {
                        case SimultaneousSpeeches(speeches) =>
                          speeches
                        case SequentialSpeeches(speeches) =>
                          speeches
                      }
                      .foldMap(speeches =>
                        Numbers(
                          speeches
                            .values
                            .toList
                            .foldMap(speech =>
                              speech
                                .content
                                .foldMap {
                                  case SpeechSegment.Quote(span) =>
                                    Utils.renderSpan(story, span).size
                                  case _ =>
                                    0
                                }
                            )
                        )
                      ) ::
                    "new quoted chars per round" ->>
                    d.debate
                      .rounds
                      .collect {
                        case SimultaneousSpeeches(speeches) =>
                          speeches
                        case SequentialSpeeches(speeches) =>
                          speeches
                      }
                      .foldLeft(Set.empty[ESpan] -> Numbers[Int](Vector())) {
                        case ((coveredSpans, prevData), speeches) =>
                          val newQuotes = speeches
                            .values
                            .toVector
                            .flatMap(speech =>
                              SpeechSegments.getNewQuotes(coveredSpans, speech.content)
                            )
                          val newQuotedAmount = newQuotes
                            .foldMap(q => Utils.renderSpan(story, q).size.toInt)

                          val newCoveredSpans = (coveredSpans ++ newQuotes)
                            .foldLeft(Set.empty[ESpan]) { case (acc, span) =>
                              acc.find(_.overlaps(span)) match {
                                case None =>
                                  acc + span
                                case Some(overlapper) =>
                                  acc - overlapper + (span |+| overlapper)
                              }
                            }

                          (newCoveredSpans, prevData |+| Numbers(newQuotedAmount))
                      }
                      ._2 ::
                    "information gain per round (bits)" ->>
                    d.debate
                      .finalJudgement
                      .foldMap { dist =>
                        def getInfo(distribution: Vector[Double]) =
                          math.log(distribution(d.debate.setup.correctAnswerIndex)) / math.log(2)
                        val priorInfo =
                          d.debate
                            .rounds
                            .collect { case JudgeFeedback(dist, _, _) =>
                              getInfo(dist)
                            }
                            .head
                        val finalInfo = getInfo(dist)
                        val infoGain  = finalInfo - priorInfo
                        val numRounds = d.debate.numDebateRounds
                        ExpectedCount(infoGain, numRounds)
                      } ::
                    "Judge reward" ->>
                    d.debate
                      .result
                      .foldMap(r => r.judgingInfo.foldMap(i => Numbers(i.judgeReward))) :: HNil)
              )
            )
          }
          .map(r =>
            r.updated(
              "proportion of quoted material redundant",
              1.0 -
                (r("new quoted chars per round").stats.mean /
                  r("all quoted chars per round").stats.mean)
            )
          )
      )
    )

    // println("Judge feedback statistics")
    // println(
    //   getMetricsString(
    //     debates.foldMap { d =>
    //       Chosen(
    //         Map(
    //           d.setting ->
    //             d.debate
    //               .setup
    //               .roles
    //               .get(Judge)
    //               .flatMap(d.debate.feedback.get)
    //               .foldMap(feedback =>
    //                 "evidence in debate (avg)" ->>
    //                   feedback
    //                     .answers
    //                     .get(Feedback.Key.ComparativeLikert("evidence in debate"))
    //                     .foldMap { case Feedback.ComparativeJudgment(first, second) =>
    //                       Numbers2((first + second) / 2.0)
    //                     } ::
    //                   "factual informativeness (total)" ->>
    //                   feedback
    //                     .answers
    //                     .get(Feedback.Key.Likert("factual informativeness (total)"))
    //                     .foldMap(Numbers2(_)) ::
    //                   "clarity (avg)" ->>
    //                   feedback
    //                     .answers
    //                     .get(Feedback.Key.ComparativeLikert("clarity"))
    //                     .foldMap { case Feedback.ComparativeJudgment(first, second) =>
    //                       Numbers2((first + second) / 2.0)
    //                     } ::
    //                   "clash (avg)" ->>
    //                   feedback
    //                     .answers
    //                     .get(Feedback.Key.ComparativeLikert("clash"))
    //                     .foldMap { case Feedback.ComparativeJudgment(first, second) =>
    //                       Numbers2((first + second) / 2.0)
    //                     } ::
    //                   "judge adaptation (avg)" ->>
    //                   feedback
    //                     .answers
    //                     .get(Feedback.Key.ComparativeLikert("judge adaptation"))
    //                     .foldMap { case Feedback.ComparativeJudgment(first, second) =>
    //                       Numbers2((first + second) / 2.0)
    //                     } :: HNil
    //               )
    //         )
    //       )
    //     }
    //   )
    // )

    // println("Debater feedback statistics")
    // println(
    //   getMetricsString(
    //     debates.foldMap { d =>
    //       Chosen(
    //         Map(
    //           d.setting ->
    //             d.debate
    //               .setup
    //               .roles
    //               .toList
    //               .collect { case (Debater(_), participant) =>
    //                 participant
    //               }
    //               .flatMap(d.debate.feedback.get)
    //               .foldMap(feedback =>
    //                 "question subjectivity" ->>
    //                   feedback
    //                     .answers
    //                     .get(Feedback.Key.Likert("subjective correctness"))
    //                     .foldMap(FewClassCount(_)) :: HNil
    //               )
    //         )
    //       )
    //     }
    //   )
    // )

    // println("Judge reward statistics")
    // println(
    //   getMetricsString(
    //     debates.foldMap { d =>
    //       Chosen(
    //         Map(
    //           d.setting ->
    //             d.debate
    //               .setup
    //               .roles
    //               .get(Judge)
    //               .foldMap { judge =>
    //                 d.debate
    //                   .result
    //                   .foldMap(result =>
    //                     result
    //                       .judgingInfo
    //                       .foldMap(info => Chosen(Map(judge -> Numbers2(info.judgeReward))))
    //                   )
    //               }
    //         )
    //       )
    //     }
    //     .map(_.map(_.stats.mean))
    //   )
    // )

    // println("Time spent judging:")
    // println(
    //   getMetricsString(
    //     debates.foldMap { d =>
    //       val judgeTimestamps = d
    //         .debate
    //         .rounds
    //         .collect { case JudgeFeedback(_, speech, _) =>
    //           speech.timestamp
    //         }
    //       val timeTakenMillis  = judgeTimestamps.max - judgeTimestamps.min
    //       val timeTakenMinutes = (timeTakenMillis / 1000L).toDouble / 60
    //       Chosen(Map(d.setting -> Numbers(timeTakenMinutes)))
    //     }
    //   )
    // )

    // analysis section

  }

}
