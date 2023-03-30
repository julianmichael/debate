package debate

import cats.Order
import cats.implicits._

import io.circe.generic.JsonCodec
import monocle.macros.Lenses

import jjm.implicits._

@Lenses
@JsonCodec
case class RuleConfig(
  name: String,
  rules: DebateRules,
  numAssignedDebaters: Int,
  numOfflineJudgesPerDebate: Int
)
object RuleConfig {
  implicit def ruleConfigOrder = Order.by[RuleConfig, String](_.name)
}

@Lenses
@JsonCodec
case class ClosingArgumentRules(maxRepeatCycles: Int, rounds: Vector[DebateRoundType])
object ClosingArgumentRules {
  def default = ClosingArgumentRules(5, Vector())
}

/** Rules of the debate. I expect to want to add more stuff here as we want to
  * vary the debates on different dimensions.
  */
@Lenses
@JsonCodec
case class DebateRules(
  fixedOpening: Vector[DebateRoundType],
  repeatingStructure: Vector[DebateRoundType],
  fixedClosing: Option[ClosingArgumentRules],
  globalQuoteRestriction: Option[Int],
  scoringFunction: ScoringFunction
) {
  def summary = {
    val mostCommonCharLimitOpt = (fixedOpening ++ repeatingStructure)
      .foldMap(_.charLimitOpt.foldMap(charLimit => Map(charLimit -> 1)))
      .toVector
      .maximaBy(_._2)
      .map(_._1)
      .maximumOption

    mostCommonCharLimitOpt.foldMap(n => s"c$n ") +
      fixedOpening.map(_.summary(mostCommonCharLimitOpt)).mkString(",") + "," +
      Option("(" + repeatingStructure.map(_.summary(mostCommonCharLimitOpt)).mkString(",") + ")")
        .filter(_ => fixedClosing.forall(_.maxRepeatCycles != 0))
        .combineAll +
      fixedClosing.fold("*")(closing =>
        (if (closing.maxRepeatCycles > 1)
           s"{1-${closing.maxRepeatCycles}}"
         else
           "") + closing.rounds.map(_.summary(mostCommonCharLimitOpt)).mkString(",")
      )
    globalQuoteRestriction.foldMap(n => s"gq$n ") + scoringFunction.summary
  }

  def roundTypes: LazyList[DebateRoundType] = {
    val end =
      fixedClosing match {
        case None =>
          LazyList.continually(repeatingStructure).flatten
        case Some(ClosingArgumentRules(maxNumRepeats, closingRounds)) =>
          LazyList.fill(maxNumRepeats)(repeatingStructure).flatten #::: LazyList.from(closingRounds)
      }
    LazyList.from(fixedOpening) #::: end
  }

  def roundTypeSet: Set[DebateRoundType] =
    fixedOpening.toSet ++ repeatingStructure.toSet ++ fixedClosing.foldMap(_.rounds)

  def hasJudge: Boolean = roundTypeSet.exists(_.hasJudge)
  def canEnd: Boolean   = fixedClosing.nonEmpty || roundTypeSet.exists(_.canEndDebate)
}
object DebateRules {

  /** Default rules. */
  def default: DebateRules = DebateRules(
    fixedOpening = Vector(
      DebateRoundType.JudgeFeedbackRound(true, 500),
      DebateRoundType.SimultaneousSpeechesRound(500, None, false),
      DebateRoundType.JudgeFeedbackRound(true, 500)
    ),
    repeatingStructure = Vector(
      DebateRoundType.SequentialSpeechesRound(500, None, false),
      DebateRoundType.JudgeFeedbackRound(true, 500)
    ),
    fixedClosing = None,
    globalQuoteRestriction = None,
    scoringFunction = ScoringFunction.LogScoreWithLinearPenalty.default
  )
}
