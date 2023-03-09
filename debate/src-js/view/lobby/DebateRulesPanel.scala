package debate
package view.lobby

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Iso
import monocle.std.{all => StdOptics}
import scalacss.ScalaCssReact._

import debate.util._

object DebateRulesPanel {

  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  val RoundTypeConfig       = SumConfig[DebateRoundType]()
  val ScoringFunctionConfig = SumConfig[ScoringFunction]()

  val defaultPerMessageQuoteLimit = 100
  val defaultGlobalQuoteLimit     = 500
  val defaultMaxRepeatedRounds    = 5

  def optionalIntInput(
    field: StateSnapshot[Option[Int]],
    labelOpt: Option[String],
    defaultInt: Int
  ) = ReactFragment(
    Checkbox2.mod(label = c"form-check-label mr-2")(
      field.zoomStateL(
        Iso[Option[Int], Boolean](_.nonEmpty)(b =>
          if (b)
            Some(defaultInt)
          else
            None
        ).asLens
      ),
      labelOpt
    ),
    NumberField2.mod(input = TagMod(c"col form-control", ^.disabled := field.value.isEmpty))(
      field.zoomStateL(Iso[Option[Int], Int](_.getOrElse(defaultInt))(Some(_)).asLens)
    )
  )

  /** Config panel for setting a list of round types. */
  def roundTypeList(roundTypes: StateSnapshot[Vector[DebateRoundType]], minItems: Int) = {
    val defaultRoundType = DebateRoundType.SequentialSpeechesRound(500, None)
    ListConfig[DebateRoundType].nice(roundTypes, defaultRoundType, minItems) {
      case ListConfig.Context(roundType, _) =>
        val rowMod = TagMod(c"form-inline")
        RoundTypeConfig
          .mod(select = TagMod(S.listCardHeaderSelect), optionsDiv = c"card-body")(roundType)(
            "Simultaneous Speeches" ->
              SumConfigOption(
                DebateRoundType.SimultaneousSpeechesRound(500, None),
                DebateRoundType.simultaneousSpeechesRound
              ) { simulSpeeches =>
                ReactFragment(
                  <.div(rowMod, c"mb-1")(
                    NumberField2(
                      simulSpeeches.zoomStateL(DebateRoundType.SimultaneousSpeechesRound.charLimit),
                      Some("Character limit")
                    )
                  ),
                  <.div(rowMod)(
                    optionalIntInput(
                      simulSpeeches
                        .zoomStateL(DebateRoundType.SimultaneousSpeechesRound.quoteLimit),
                      Some("Quote character limit"),
                      defaultPerMessageQuoteLimit
                    )
                  )
                )
              },
            "Sequential Speeches" ->
              SumConfigOption(
                DebateRoundType.SequentialSpeechesRound(500, None),
                DebateRoundType.sequentialSpeechesRound
              ) { seqSpeeches =>
                ReactFragment(
                  <.div(rowMod, c"mb-1")(
                    NumberField2(
                      seqSpeeches.zoomStateL(DebateRoundType.SequentialSpeechesRound.charLimit),
                      labelOpt = Some("Character limit")
                    )
                  ),
                  <.div(rowMod)(
                    optionalIntInput(
                      seqSpeeches.zoomStateL(DebateRoundType.SequentialSpeechesRound.quoteLimit),
                      Some("Quote character limit"),
                      defaultPerMessageQuoteLimit
                    )
                  )
                )
              },
            "Judge Feedback" ->
              SumConfigOption(
                DebateRoundType.JudgeFeedbackRound(true, 500),
                DebateRoundType.judgeFeedbackRound
              ) { judgeFeedback =>
                ReactFragment(
                  <.div(rowMod, c"mb-1")(
                    NumberField2(
                      judgeFeedback.zoomStateL(DebateRoundType.JudgeFeedbackRound.charLimit),
                      labelOpt = Some("Character limit")
                    )
                  ),
                  <.div(rowMod)(
                    Checkbox2(
                      judgeFeedback.zoomStateL(DebateRoundType.JudgeFeedbackRound.reportBeliefs),
                      Some("Report probabilities")
                    )
                  )
                )
              },
            "Negotiate End" ->
              SumConfigOption(DebateRoundType.NegotiateEndRound, DebateRoundType.negotiateEnd) {
                _ => <.div()
              }
          )
    }
  }

  def roundsConfig(label: String, minItems: Int, rounds: StateSnapshot[Vector[DebateRoundType]]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)(label),
      <.div(S.inputRowContents)(roundTypeList(rounds, minItems = minItems))
    )

  def closingRoundsConfig(closingRules: StateSnapshot[Option[ClosingArgumentRules]]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Closing Rounds"),
      PartialStateBackup[Option[ClosingArgumentRules], ClosingArgumentRules](
        state = closingRules,
        initialPartValue = ClosingArgumentRules.default,
        partOptional = StdOptics.some.asOptional
      ) { (storedArgRules, syncedArgRulesOpt) =>
        <.div(S.inputRowContents)(
          <.div(c"form-inline", c"mb-1".when(closingRules.value.nonEmpty))(
            Checkbox2.mod(label = c"form-check-label mr-2")(
              closingRules.zoomStateL(
                Iso[Option[ClosingArgumentRules], Boolean](_.nonEmpty)(b =>
                  if (b)
                    Some(storedArgRules.value)
                  else
                    None
                ).asLens
              ),
              Some("Maximum number of cycles")
            ),
            NumberField2
              .mod(input = TagMod(c"col form-control", ^.disabled := closingRules.value.isEmpty))(
                syncedArgRulesOpt
                  .getOrElse(storedArgRules)
                  .zoomStateL(ClosingArgumentRules.maxRepeatCycles)
              )
          ),
          syncedArgRulesOpt.map(rules =>
            roundTypeList(rules.zoomStateL(ClosingArgumentRules.rounds), minItems = 0)
          )
        )
      }
    )

  def scoringFunctionConfig(scoringFunction: StateSnapshot[ScoringFunction]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Judge Scoring Function"),
      <.div(S.inputRowContents)(
        ScoringFunctionConfig(scoringFunction)(
          "Spherical Score" ->
            SumConfigOption(
              ScoringFunction.SphericalScoreWithLinearPenalty(3, 1),
              ScoringFunction.sphericalScoreWithLinearPenalty
            ) { sphericalScore =>
              ReactFragment(
                V.LiveTextField
                  .Double(
                    sphericalScore
                      .zoomStateL(ScoringFunction.SphericalScoreWithLinearPenalty.baseCoefficient),
                    Some("Base coefficient")
                  ),
                V.LiveTextField
                  .Double(
                    sphericalScore
                      .zoomStateL(ScoringFunction.SphericalScoreWithLinearPenalty.perTurnPenalty),
                    Some("Per turn penalty")
                  )
              )
            },
          "Quadratic Score" ->
            SumConfigOption(
              ScoringFunction.QuadraticScoreWithLinearPenalty(3, 1),
              ScoringFunction.quadraticScoreWithLinearPenalty
            ) { quadraticScore =>
              ReactFragment(
                V.LiveTextField
                  .Double(
                    quadraticScore
                      .zoomStateL(ScoringFunction.QuadraticScoreWithLinearPenalty.baseCoefficient),
                    Some("Base coefficient")
                  ),
                V.LiveTextField
                  .Double(
                    quadraticScore
                      .zoomStateL(ScoringFunction.QuadraticScoreWithLinearPenalty.perTurnPenalty),
                    Some("Per turn penalty")
                  )
              )
            },
          "Log Score" ->
            SumConfigOption(
              ScoringFunction.LogScoreWithLinearPenalty.default,
              ScoringFunction.logScoreWithLinearPenalty
            ) { logScore =>
              ReactFragment(
                V.LiveTextField
                  .Double(
                    logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.baseCoefficient),
                    Some("Base coefficient")
                  ),
                V.LiveTextField
                  .Double(
                    logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.constant),
                    Some("Constant")
                  ),
                V.LiveTextField
                  .Double(
                    logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.logBase),
                    Some("Log base")
                  ),
                V.LiveTextField
                  .Double(
                    logScore.zoomStateL(ScoringFunction.LogScoreWithLinearPenalty.perTurnPenalty),
                    Some("Per turn penalty")
                  )
              )
            }
        )
      )
    )

  def globalQuoteRestrictionConfig(quoteRestriction: StateSnapshot[Option[Int]]) =
    <.div(S.mainLabeledInputRow)(
      <.div(S.inputRowLabel)("Global Evidence Restrictions"),
      <.div(S.inputRowContents)(
        <.div(c"form-inline")(
          optionalIntInput(quoteRestriction, Some("Quote character limit"), defaultGlobalQuoteLimit)
        )
      )
    )

  def apply(
    rules: StateSnapshot[DebateRules]
    // lobby: Lobby,
    // qualityService: QuALITYService[AsyncCallback],
  ) = ReactFragment(
    roundsConfig("Opening Rounds", 0, rules.zoomStateL(DebateRules.fixedOpening)),
    roundsConfig("Repeated Rounds", 1, rules.zoomStateL(DebateRules.repeatingStructure)),
    closingRoundsConfig(rules.zoomStateL(DebateRules.fixedClosing)),
    globalQuoteRestrictionConfig(rules.zoomStateL(DebateRules.globalQuoteRestriction)),
    scoringFunctionConfig(rules.zoomStateL(DebateRules.scoringFunction))
  )

}
