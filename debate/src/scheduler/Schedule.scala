package debate
package scheduler

import cats.implicits._
import cats.kernel.Order

import monocle.macros.Lenses

import jjm.implicits._

import debate.util.SparseDistribution

@Lenses
case class Schedule(
  desiredWorkload: SparseDistribution[String],
  desiredRules: SparseDistribution[RuleConfig],
  complete: Vector[DebateSetup],
  incomplete: Vector[DebateSetup],
  novel: Vector[DebateSetup]
) {
  import Schedule._

  lazy val allIncomplete = incomplete ++ novel
  lazy val all           = complete ++ allIncomplete
  lazy val allPeople = all.foldMap(setup => setup.roles.values.toSet ++ setup.offlineJudges.keySet)

  def forStory(storyId: SourceMaterialId) = all
    .filter(setup => SourceMaterialId.fromSourceMaterial(setup.sourceMaterial) == storyId)

  // Map[person, Map[factor -> load]]
  def computeImbalance[A: Order](loadPerDebater: Map[String, Map[A, Double]]) = {
    val balancePerDebater = loadPerDebater.mapVals(m => SparseDistribution.fromMap(m).get)
    val totalBalance      = SparseDistribution.fromMap(loadPerDebater.unorderedFold).get

    balancePerDebater.unorderedFoldMap(distance(_, totalBalance)) / balancePerDebater.size
  }

  def computeImbalanceFromUniform(loadPerDebater: Map[String, Map[String, Double]]) = {
    val balancePerDebater = loadPerDebater.mapVals(m => SparseDistribution.fromMap(m).get)
    val totalBalance =
      SparseDistribution.fromMap(loadPerDebater.unorderedFold.mapVals(_ => 1.0)).get
    balancePerDebater.unorderedFoldMap(distance(_, totalBalance)) / balancePerDebater.size
  }

  // slightly superlinear
  def distance[A](
    x: SparseDistribution[A],
    y: SparseDistribution[A],
    exponent: Double = 1.2
  ): Double = (x.probs.map(-_) |+| y.probs).unorderedFoldMap(x => math.pow(math.abs(x), exponent))

  def workloadCounts = {
    // imbalances in load should be penalized differently depending on source
    // (e.g., judging an extra debate offline is less work than reading a new story)
    val storyWeight          = 1.0
    val debateWeight         = 1.0
    val liveJudgingWeight    = 0.5
    val offlineJudgingWeight = 0.25

    val stories = allIncomplete
      .foldMap(a => a.debaters.map(_ -> Set(a.storyId)).toMap)
      .mapVals(_.size.toDouble * storyWeight)
    val debates = allIncomplete.foldMap(assignment =>
      assignment.debaters.unorderedFoldMap(d => Map(d -> debateWeight)) |+|
        assignment.judge.foldMap(j => Map(j -> liveJudgingWeight)) |+|
        assignment.offlineJudges.keySet.unorderedFoldMap(j => Map(j -> offlineJudgingWeight))
    )
    stories |+| debates
  }
  lazy val workload = SparseDistribution
    .fromMap(workloadCounts)
    .getOrElse(SparseDistribution.uniform(allPeople.toNes.get))

  // assign the right amount of debating to the right people
  def workloadImbalance = distance(workload, desiredWorkload)

  val ruleCounts = {
    val configs = desiredRules.probs.keys.toList
    novel.flatMap(setup => configs.find(_.rules == setup.rules)).counts
  }
  val rulesDistOpt = SparseDistribution.fromMap(ruleCounts.mapVals(_.toDouble))
  // distribute novel assignments according to rules distribution
  def rulesImbalance = rulesDistOpt.foldMap(rulesDist => distance(rulesDist, desiredRules))

  // assign the right amount of work to the right people
  // def workloadImbalance = distance(workload, desiredWorkload)

  // minimize the number of times people have to read stories (concentrating debaters)
  def storiesRead = all
    .foldMap(a => a.debaters.map(_ -> Set(a.storyId)).toMap)
    .unorderedFoldMap(_.size.toDouble)

  // everyone should debate/judge live/judge offline in similar proportions in aggregate
  def roleLoad = all.foldMap(assignment =>
    assignment.debaters.unorderedFoldMap(d => Map(d -> Map("debater" -> 1.0))) |+|
      assignment
        .offlineJudges
        .keySet
        .unorderedFoldMap(d => Map(d -> Map("judge (offline)" -> 1.0))) |+|
      assignment.judge.foldMap(j => Map(j -> Map("judge (live)" -> 1.0)))
  )
  def roleDistributionPerDebater = roleLoad.mapVals(m => SparseDistribution.fromMap(m).get)
  def roleDistributionGlobal     = SparseDistribution.fromMap(roleLoad.unorderedFold).get
  def roleImbalance              = computeImbalance(roleLoad)

  // minimize imbalance of honesty/dishonesty
  def honestyLoad = all.foldMap(assignment =>
    assignment
      .honestDebaterOpt
      .foldMap(honestDebater => Map(honestDebater -> Map("honest" -> 1.0))) |+|
      assignment
        .dishonestDebaterOpt
        .foldMap(dishonestDebater => Map(dishonestDebater -> Map("dishonest" -> 1.0)))
  )
  def honestyImbalance = computeImbalance(honestyLoad)

  def orderLoad = all.foldMap(assignment =>
    assignment
      .honestDebaterOpt
      .foldMap(honestDebater => Map(honestDebater -> Map(assignment.correctAnswerIndex -> 1.0))) |+|
      assignment
        .dishonestDebaterOpt
        .foldMap(dishonestDebater =>
          Map(dishonestDebater -> Map((1 - assignment.correctAnswerIndex) -> 1.0))
        )
  )

  def orderImbalance = computeImbalance(orderLoad)

  // maximize debater spread:
  def opponentLoad = all.foldMap(assignment =>
    // rewritten to account for honestDebaterOpt and dishonestDebaterOpt
    (assignment.honestDebaterOpt, assignment.dishonestDebaterOpt)
      .mapN { (honestDebater, dishonestDebater) =>
        Map(honestDebater      -> Map(dishonestDebater -> 1.0)) |+|
          Map(dishonestDebater -> Map(honestDebater -> 1.0))
      }
      .combineAll
  )
  def opponentImbalance = computeImbalanceFromUniform(opponentLoad)

  // maximize judge -> debater / debater -> judge spread:
  def judgeDebaterLoad = all.foldMap(assignment =>
    assignment
      .debaters
      .unorderedFoldMap(d =>
        (assignment.offlineJudges.keySet ++ assignment.judge)
          .unorderedFoldMap(j => Map(j -> Map(d -> 1.0)))
      )
  )

  def debaterJudgeLoad = all.foldMap(assignment =>
    assignment
      .debaters
      .unorderedFoldMap(d =>
        (assignment.offlineJudges.keySet ++ assignment.judge)
          .unorderedFoldMap(j => Map(d -> Map(j -> 1.0)))
      )
  )

  def judgeDebaterImbalance =
    (computeImbalanceFromUniform(judgeDebaterLoad) +
      computeImbalanceFromUniform(debaterJudgeLoad)) / 2

  // // maximize offline judge -> debater / debater -> offline judge spread:
  // def offlineJudgeDebaterLoadImbalance =
  //   (computeImbalanceFromUniform(assignment =>
  //     assignment
  //       .debaters
  //       .unorderedFoldMap(d =>
  //         assignment.offlineJudges.keySet.unorderedFoldMap(j => Map(j -> Map(d -> 1.0)))
  //       )
  //   ) +
  //     computeImbalanceFromUniform(assignment =>
  //       assignment
  //         .debaters
  //         .unorderedFoldMap(d =>
  //           assignment.offlineJudges.keySet.unorderedFoldMap(j => Map(d -> Map(j -> 1.0)))
  //         )
  //     )) / 2

  // strongly penalize judging the same story more than once
  // (exponent strengthens penalty)
  // this should only affect balance of 1 judging / 2 judgings
  def judgingsPerStory(exponent: Double = 2): Double = all
    .foldMap { assignment =>
      Map(assignment.storyId -> assignment.judges.unorderedFoldMap(j => Map(j -> 1)))
    }
    .unorderedFoldMap(_.unorderedFoldMap(math.pow(_, exponent)))

  lazy val cost: Double = {
    import DebateScheduler.Params._
    val terms = List(
      workloadImbalance * workloadMultiplier,
      storiesRead,
      // roleImbalance,
      honestyImbalance,
      orderImbalance,
      opponentImbalance,
      judgeDebaterImbalance,
      // offlineJudgeDebaterLoadImbalance,
      judgingsPerStory(exponent = 2)
    )
    terms.sum
  }
}
object Schedule {
  implicit class DebateSetupExtensions(val setup: DebateSetup) {
    def storyId: SourceMaterialId        = SourceMaterialId.fromSourceMaterial(setup.sourceMaterial)
    def honestDebaterOpt: Option[String] = setup.roles.get(Debater(setup.correctAnswerIndex))
    def dishonestDebaterOpt: Option[String] = setup.roles.get(Debater(1 - setup.correctAnswerIndex))
    def judge: Option[String]               = setup.roles.get(Judge)
    // def offlineJudges: Set[String],

    def debaters: Set[String] = Set(dishonestDebaterOpt, honestDebaterOpt).flatten
    def judges                = setup.offlineJudges.keySet ++ judge
    def allParticipants       = debaters ++ judges

    def isAssigned(debater: String): Boolean = allParticipants.contains(debater)
  }

}
