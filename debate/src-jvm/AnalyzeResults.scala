package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import io.circe.generic.JsonCodec
import monocle.macros.Lenses
import debate.quality.QuALITYStory
import debate.JudgingResult
import cats.kernel.Monoid

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

  @JsonCodec
  @Lenses
  case class DebateSetting(isHuman: Boolean, isDebate: Boolean)
  object DebateSetting {
    implicit def debateSettingShow = cats
      .Show
      .show[DebateSetting] { setting =>
        val human =
          if (setting.isHuman)
            "Human"
          else
            "AI"
        val debate =
          if (setting.isDebate)
            "debate"
          else
            "consultancy"
        s"$human $debate"
      }
  }

  def getSetting(debate: Debate): DebateSetting = {
    val isAI = debate.setup.roles.values.toList.exists(_ == "GPT-4")
    val isDebate =
      debate
        .setup
        .roles
        .keySet
        .collect { case Debater(i) =>
          i
        }
        .size > 1
    DebateSetting(isHuman = !isAI, isDebate = isDebate)
  }

  case class AnalyzedOnlineDebate(
    roomName: String,
    debate: Debate,
    endTime: Long,
    result: JudgingResult,
    questionDifficultyOpt: Option[Int]
  ) {
    def probabilityCorrect     = result.finalJudgement(debate.setup.correctAnswerIndex)
    val setting: DebateSetting = getSetting(debate)
    def original               = AnalyzedDebate(roomName, debate, questionDifficultyOpt)
  }

  case class AnalyzedDebate(roomName: String, debate: Debate, questionDifficultyOpt: Option[Int]) {
    def isNotTooLittleContextRequired = questionDifficultyOpt.forall(_ > 1)
    val onlineJudgment                = debate.result.flatMap(_.judgingInfo)
    val isOnline               = onlineJudgment.exists(_ => debate.setup.roles.contains(Judge))
    val setting: DebateSetting = getSetting(debate)
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

  import jjm.metrics._
  import jjm.implicits._
  import shapeless._
  import shapeless.syntax.singleton._
  import shapeless.record._

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
  }

  def analyzePerformance(debates: List[AnalyzedOnlineDebate]) = {
    val metrics = debates.foldMap(d =>
      Chosen(
        Map(
          d.setting ->
            ("accuracy" ->> correctIf(d, d.probabilityCorrect > 0.5) ::
              "length" ->> Numbers(d.debate.numDebateRounds) ::
              "reward" ->> Numbers(d.result.judgeReward) ::
              "correct speaker" ->> FewClassCount(d.result.correctAnswerIndex) ::
              "winning speaker" ->>
              FewClassCount(d.result.finalJudgement.zipWithIndex.maxBy(_._1)._2) :: HNil)
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

  def analyzeDebates(
    allDebates: List[(String, Debate)],
    quality: Map[String, QuALITYStory]
  ): IO[Unit] = IO {
    val unbalancedDebates = allDebates
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

    val debates =
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
        .transform { case (setting, debates) =>
          if (setting.isDebate)
            debates
          else {
            // balance consultancies
            val numHonest     = debates.count(isConsultantHonest)
            val numDishonest  = debates.size - numHonest
            val difference    = math.abs(numHonest - numDishonest)
            val hasMoreHonest = numHonest > numDishonest

            // deterministically (randomly) shuffle debates
            val debatesByQuestion = debates.groupBy(d => getQuestionId(d.debate))
            val rand              = new scala.util.Random(876)
            val commonDebates = rand.shuffle(
              debatesByQuestion
                .filter(p =>
                  p._2.exists(isConsultantHonest) && p._2.exists(d => !isConsultantHonest(d))
                )
                .toList
                .sortBy(_._1)
                .map(p => p.copy(_2 = p._2.sortBy(_.endTime))) // prefer later debates
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

    println("Longest debates:")
    println(
      getMetricsString(
        debates
          .foldMap(d => Chosen(Map(d.setting -> Vector(d))))
          .map(
            _.sortBy(-_.debate.numDebateRounds)
              .take(5)
              .map(d => s"${d.roomName} (${d.debate.numDebateRounds})")
              .toVector
          )
      )
    )

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

    import jjm.ling.Text

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

    println("Work contributions:")
    val workContributions = debates
      .map(_.debate)
      .foldMap(DebateStats.fromDebate)
      .mapKeys(_.toString)
      .map(_.map(_.wins.total))
    println(getMetricsString(workContributions))
    println(getMetricsString(workContributions.map(_.data.values.toList.foldMap(Numbers(_)))))

    // write a CSV with room names and judge names to get the repeatable sample of the final filtered debates
    import com.github.tototoshi.csv._
    import java.io.File
    CSVWriter
      .open(new File("sample-rooms.csv"))
      .writeAll(debates.map(d => List(d.roomName, d.debate.setup.roles(Judge))))
  }

}
