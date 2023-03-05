package debate
package scheduler

import cats.implicits._

import jjm.implicits._

import debate.util.SparseDistribution

case class Schedule(
  desiredWorkload: SparseDistribution[String],
  complete: Vector[DebateSetup],
  incomplete: Vector[DebateSetup],
  novel: Vector[DebateSetup]
) {
  import Schedule._

  lazy val allIncomplete = incomplete ++ novel
  lazy val all           = complete ++ allIncomplete

  lazy val workload: SparseDistribution[String] = {
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
        Map(assignment.judge -> liveJudgingWeight) |+|
        assignment.offlineJudges.keySet.unorderedFoldMap(j => Map(j -> offlineJudgingWeight))
    )
    SparseDistribution(stories |+| debates)
  }

  // Map[person, Map[factor -> load]]
  def computeImbalance[A](computeLoad: DebateSetup => Map[String, Map[A, Double]]) = {
    val loadPerDebater    = all.foldMap(computeLoad)
    val balancePerDebater = loadPerDebater.mapVals(SparseDistribution(_))
    val totalBalance      = SparseDistribution(loadPerDebater.unorderedFold)

    balancePerDebater.unorderedFoldMap(distance(_, totalBalance)) / balancePerDebater.size
  }

  def computeImbalanceFromUniform(computeLoad: DebateSetup => Map[String, Map[String, Double]]) = {
    val loadPerDebater    = all.foldMap(computeLoad)
    val balancePerDebater = loadPerDebater.mapVals(SparseDistribution(_))
    val totalBalance      = SparseDistribution(loadPerDebater.unorderedFold.mapVals(_ => 1.0))
    balancePerDebater.unorderedFoldMap(distance(_, totalBalance)) / balancePerDebater.size
  }

  // slightly superlinear
  def distance[A](
    x: SparseDistribution[A],
    y: SparseDistribution[A],
    exponent: Double = 1.2
  ): Double = (x.probs.mapVals(-_) |+| y.probs)
    .unorderedFoldMap(x => math.pow(math.abs(x), exponent))

  // assign the right amount of work to the right people
  def workloadImbalance = distance(workload, desiredWorkload)

  // minimize the number of times people have to read stories (concentrating debaters)
  def storiesRead = all
    .foldMap(a => a.debaters.map(_ -> Set(a.storyId)).toMap)
    .unorderedFoldMap(_.size.toDouble)

  // everyone should debate/judge live/judge offline in similar proportions in aggregate
  def roleImbalance = computeImbalance(assignment =>
    assignment.debaters.unorderedFoldMap(d => Map(d -> Map("debater" -> 1.0))) |+|
      assignment
        .offlineJudges
        .keySet
        .unorderedFoldMap(d => Map(d -> Map("judge (offline)" -> 1.0))) |+|
      Map(assignment.judge -> Map("judge (live)" -> 1.0))
  )

  // minimize imbalance of honesty/dishonesty
  def honestyImbalance = computeImbalance(assignment =>
    Map(assignment.honestDebater      -> Map("honest" -> 1.0)) |+|
      Map(assignment.dishonestDebater -> Map("dishonest" -> 1.0))
  )

  def orderImbalance = computeImbalance(assignment =>
    Map(assignment.honestDebater      -> Map(assignment.honestFirst -> 1.0)) |+|
      Map(assignment.dishonestDebater -> Map(!assignment.honestFirst -> 1.0))
  )

  // maximize debater spread:
  def opponentImbalance = computeImbalanceFromUniform(assignment =>
    Map(assignment.honestDebater      -> Map(assignment.dishonestDebater -> 1.0)) |+|
      Map(assignment.dishonestDebater -> Map(assignment.honestDebater -> 1.0))
  )

  // maximize judge -> debater / debater -> judge spread:
  def judgeDebaterLoadImbalance =
    (computeImbalanceFromUniform(assignment =>
      assignment.debaters.unorderedFoldMap(d => Map(assignment.judge -> Map(d -> 1.0)))
    ) +
      computeImbalanceFromUniform(assignment =>
        assignment.debaters.unorderedFoldMap(d => Map(assignment.judge -> Map(d -> 1.0)))
      )) / 2

  // maximize offline judge -> debater / debater -> offline judge spread:
  def offlineJudgeDebaterLoadImbalance =
    (computeImbalanceFromUniform(assignment =>
      assignment
        .debaters
        .unorderedFoldMap(d =>
          assignment.offlineJudges.keySet.unorderedFoldMap(j => Map(j -> Map(d -> 1.0)))
        )
    ) +
      computeImbalanceFromUniform(assignment =>
        assignment
          .debaters
          .unorderedFoldMap(d =>
            assignment.offlineJudges.keySet.unorderedFoldMap(j => Map(d -> Map(j -> 1.0)))
          )
      )) / 2

  // strongly penalize judging the same story more than once
  // (exponent strengthens penalty)
  // this should only affect balance of 1 judging / 2 judgings
  def judgingsPerStory(exponent: Double = 2): Double = all
    .foldMap { assignment =>
      Map(assignment.storyId -> assignment.judges.unorderedFoldMap(j => Map(j -> 1)))
    }
    .unorderedFoldMap(_.unorderedFoldMap(math.pow(_, exponent)))

  /** result is non-negative */
  lazy val cost: Double = {
    val terms = List(
      workloadImbalance,
      storiesRead,
      roleImbalance,
      honestyImbalance,
      orderImbalance,
      opponentImbalance,
      judgeDebaterLoadImbalance,
      offlineJudgeDebaterLoadImbalance,
      judgingsPerStory(exponent = 2)
    )
    terms.sum
  }
}
object Schedule {
  implicit class DebateSetupExtensions(val setup: DebateSetup) {
    def storyId: SourceMaterialId = SourceMaterialId.fromSourceMaterial(setup.sourceMaterial)
    def honestDebater: String     = setup.roles(Debater(setup.correctAnswerIndex))
    def dishonestDebater: String  = setup.roles(Debater(1 - setup.correctAnswerIndex))
    def judge: String             = setup.roles(Judge) // TODO
    // def offlineJudges: Set[String],
    def honestFirst: Boolean = setup.correctAnswerIndex == 0

    def debaters        = Set(dishonestDebater, honestDebater)
    def judges          = setup.offlineJudges.keySet + judge
    def allParticipants = debaters ++ judges

    def isAssigned(debater: String): Boolean = allParticipants.contains(debater)
  }

}
