package debate

import scala.annotation.nowarn
import scala.language.postfixOps

import scalacss.DevDefaults._
import scalacss.internal.Attr
import scalacss.internal.ValueT

object gridGap extends ValueT.TypedAttrT1[ValueT.Len] with ValueT.ZeroLit {
  override val attr = Attr.real("grid-gap")
}

/** Stylesheet for the webapp. Inherits default styles for the default view
  * components; these can be overridden.
  */
object Styles extends jjm.ui.View.Styles {
  import dsl._

  val niceGreen = c"#28a745"

  val commonBorderRadius = 5 px
  val mainHalfPadding    = 3 px
  val spaceyPadding      = 6 px
  val hoverBgColor       = c"#eee"

  val customSelect = style(addClassNames("custom-select"), flexGrow(1))

  // overrides

  override val textFieldSpan = style(display.flex, flexDirection.row, flexGrow(1))

  val inputLabel = style(
    addClassNames("col-form-label"),
    width(160 px),
    textAlign.right,
    marginRight(mainHalfPadding)
  )

  override val textFieldLabel = inputLabel

  override val textFieldInput = style(addClassNames("form-control"))

  override val intArrowFieldInput = style(width(60 px))

  // list config

  val listConfigListDiv = style(textAlign.left)
  // val listConfigRemoveItemSpan = style(color.gray, &.hover(color.black, fontWeight.bold))
  val listConfigAddItemDiv = style(
    // textAlign.left
  )
  // val listConfigAddItemSpan = listConfigRemoveItemSpan

  // sum config

  val sumConfigOuterDiv   = style()
  val sumConfigInnerDiv   = style()
  val sumConfigSelect     = customSelect
  val sumConfigOptionsDiv = style()
  // style(
  //   addClassNames("custom-select")
  // )
  val listCardHeaderSelect = style(
    addClassNames("custom-select", "card-header"),
    // border.none,
    borderTop.none,
    borderLeft.none,
    borderRight.none,
    borderBottomLeftRadius(0 rem),
    borderBottomRightRadius(0 rem),
    padding(0 rem, 0 rem, 0 rem, 15 px),
    // TODO maybe extract into another style for when there's something on the left
    borderTopLeftRadius(0 rem)
    // TODO border top right radius media query for when screen is small
  )

  // prob sliders

  val probSlidersDiv = style()

  // overall styles

  def quotedSeparatedStrings(xs: String*) = xs.map(x => "\"" + x + "\"").mkString(", ")

  style(unsafeRoot("html")(fontSize(14 px)))

  style(
    unsafeRoot("body")(
      margin.`0`,
      fontFamily :=!
        quotedSeparatedStrings(
          "-apple-system",
          "BlinkMacSystemFont",
          "Segoe UI",
          "Roboto",
          "Oxygen",
          "Ubuntu",
          "Cantarell",
          "Fira Sans",
          "Droid Sans",
          "Helvetica Neue",
          "sans-serif"
        )
    )
  )

  style(
    unsafeRoot("code")(
      fontFamily :=!
        quotedSeparatedStrings(
          "source-code-pro",
          "Menlo",
          "Monaco",
          "Consolas",
          "Courier New",
          "monospace"
        )
    )
  )

  // App

  // generic arrangement styles

  val row        = style(display.flex, flexDirection.row)
  val rowWithGap = style(display.flex, flexDirection.row, gap(1 rem))

  val col = style(display.flex, flexDirection.column)

  val grow = style(flexGrow(1))

  val hidden = style(visibility.hidden)

  // generic coloring/appearance styles

  val veryGreyedOut = style(opacity(0.25))

  val simpleUnselectable = style(opacity(0.5))

  val simpleSelectable = style(cursor.pointer, &.hover(filter := "brightness(85%)"))

  val simpleSelectableText = style(cursor.pointer, &.hover(fontStyle.italic))

  val bold = style(fontWeight.bold)

  val simpleSelected = style(backgroundColor(c"#ddd"))

  val spaceyContainer = style(
    paddingTop(spaceyPadding),
    paddingLeft(spaceyPadding / 2),
    paddingRight(spaceyPadding / 2),
    unsafeChild("> *") {
      style(
        marginLeft(spaceyPadding / 2),
        marginRight(spaceyPadding / 2),
        marginBottom(spaceyPadding)
      )
    }
  )
  val spaceySubcontainer = style(
    margin(0 em).important,
    unsafeChild("> *") {
      style(
        marginLeft(spaceyPadding / 2),
        marginRight(spaceyPadding / 2),
        marginBottom(spaceyPadding)
      )
    }
  )

  import scalacss.internal.Attr
  object accentColor extends ValueT.TypedAttr_Color {
    override val attr = Attr.real("accent-color")
  }

  val app = style(
    position.absolute,
    top.`0`,
    left.`0`,
    height(100 vh),
    width(100 vw),
    overflowX.hidden
  )

  val awaitingFeedbackStatusLabel          = style(color.purple)
  val inProgressStatusLabel                = style(color.darkorange)
  val assignedForOfflineJudgingStatusLabel = style(color.gold)
  val currentlyOfflineJudgingStatusLabel   = style(color.darkorange)
  val eligibleForOfflineJudgingStatusLabel = style(color.black)
  val waitingToBeginStatusLabel            = style(color.gold)
  val mustJudgeBeforeSeeingStatusLabel     = style(color.red)
  val completeStatusLabel                  = style(color.darkgreen)

  val loading = style(
    gridRow    := "1/-1",
    gridColumn := "1/-1",
    display.flex,
    justifyContent.center,
    alignItems.center
  )

  val lobbyContainer = style(addClassNames("container"))

  val debateContainer = style(
    addClassNames("container-fluid"),
    height(100 %%),
    display.flex,
    flexDirection.column
  )

  val userInfoMessage = style(margin.auto)

  val roomRolesRow = style(
    // flexGrow(1),
    width(100 %%),
    display.flex,
    flexDirection.row
    // padding(mainHalfPadding),
  )

  val facilitatorName = style(fontStyle.italic, color.dodgerblue)

  val optionBox = style(
    flexBasis := "0",
    flexGrow(1),
    minHeight(4 rem),
    // margin(mainHalfPadding),
    padding(spaceyPadding / 3),
    borderStyle.solid,
    borderWidth(1 px),
    borderColor(c"#ddd"),
    borderRadius(commonBorderRadius)
  )

  val optionTitle = style(margin(spaceyPadding / 3), fontWeight.bold)

  val metadataListContainer = style(
    display.grid,
    gridTemplateColumns := "repeat(auto-fill,minmax(16rem, 1fr))"
  )

  val lobbyPresenceIndicator = style(fontSize(7 px), position.absolute, top(.5 rem), right(.5 rem))

  val lobbyPresenceCircle = style(addClassNames("pl-1"))

  val judgmentBarLabel = style(fontSize(0.5 rem), width(3.5 rem))
  val judgmentBar = style(
    row,
    overflow.hidden,
    fontSize(0.5 rem),
    height(0.7 rem),
    borderRadius(0.5 rem)
  )

  val inRoundProbabilityBar = style(
    addClassNames("mt-1"),
    height(1.5 rem),
    fontSize(1 rem),
    borderRadius(commonBorderRadius)
  )

  val attentionBackground = style(backgroundColor(rgba(255, 255, 0, 0.25)))

  val profileListContainer = style(
    display.grid,
    gridTemplateColumns := "repeat(auto-fill,minmax(20rem, 1fr))"
  )

  val storyListContainer = style(
    display.grid,
    gridTemplateColumns := "repeat(auto-fill,minmax(10rem, 1fr))",
    gridGap(spaceyPadding)
  )

  val judgeColor      = darkgreen
  val judgeColorLight = green

  val answerColors = Vector(darkblue, darkred, rebeccapurple, darkorange, darkturquoise)

  val answerColorsLight = Vector(
    lightblue,
    lightpink,
    lightsteelblue,
    lightgoldenrodyellow,
    lightseagreen
  )

  val judgeAssignment   = style(color(judgeColor))
  val debaterAssignment = styleF.int(0 to 4)(i => styleS(color(answerColors(i))))

  val debateWidthOffset =
    styleF.int(0 to 4)(i =>
      styleS(
        width(75 %%),
        if (i % 2 == 0)
          alignSelf.start
        else
          alignSelf.end
      )
    )

  val pendingBg = style(backgroundColor.coral)

  val offlineJudgeBg  = style(backgroundColor(c"#eee"), color.black)
  val judgeFeedbackBg = style(backgroundColor(judgeColor), color.white)
  val answerBg = styleF.int(0 to 4)(i => styleS(backgroundColor(answerColors(i)), color.white))

  val noRoleBg      = style()
  val facilitatorBg = style(backgroundColor.aliceblue, color.black)
  val observerBg    = style(backgroundColor(c"#eee"), color.black)
  val judgeBg       = style(backgroundColor(judgeColorLight), color.white)

  val judgeDecision = style(borderStyle.solid, borderWidth(2 px), borderColor.red)
  val debaterBg =
    styleF.int(0 to 4)(i => styleS(backgroundColor(answerColorsLight(i)), color.white))

  val answerOutline =
    styleF.int(0 to 4)(i =>
      styleS(borderStyle.solid, borderColor(answerColors(i)), color(answerColors(i)))
    )

  val noRoleOutline      = style()
  val facilitatorOutline = style(borderStyle.solid, borderColor.aliceblue, color.black)
  val observerOutline    = style(borderStyle.solid, borderColor(c"#eee"), color.black)
  val judgeOutline       = style(borderStyle.solid, borderColor.darkgreen, color.darkgreen)

  val debateColumn = style(
    // position.relative,
    // margin.auto,
    // padding(10 px),
    // width(100 %%),
    // height(100 %%),
    display.flex,
    flexDirection.column,
    borderRadius(commonBorderRadius),
    overflow.hidden
  )

  val facilitatorColumn = style(
    display.flex,
    flexDirection.column,
    borderRadius(commonBorderRadius)
  )

  val stickyBanner = style(position.sticky, top.`0`, backgroundColor.white, zIndex(10))

  val debateHeaderRowCol = style(overflow.hidden, borderRadius(commonBorderRadius))

  val inDebateRoleBox = style(
    flexBasis := "0",
    flexGrow(1),
    // width(100 %%),
    fontSize(12 pt),
    textAlign.left,
    // margin(mainHalfPadding),
    padding(mainHalfPadding)
  )

  val questionBox        = style(inDebateRoleBox, color(judgeColor), backgroundColor.white)
  val questionBoxCurrent = style(inDebateRoleBox, color.white, backgroundColor(judgeColor))

  val questionTitle = style(fontWeight.bold)

  val judgesLabel = style(fontWeight.bold)

  val judgesList = style()

  val missingParticipant = style(fontWeight.lighter, fontStyle.italic)
  val presentParticipant = style(fontWeight.bold)

  val answerBoxesRow = style(display.flex, flexDirection.row)

  val answerBox =
    styleF.int(0 to 4)(i => styleS(inDebateRoleBox, color(answerColors(i)), backgroundColor.white))
  val answerBoxCurrent =
    styleF.int(0 to 4)(i =>
      styleS(
        // flexGrow(1),
        inDebateRoleBox,
        color.white,
        answerBg(i)
      )
    )

  val answerTitle = style(fontWeight.bold)

  val offlineJudgesBox = style(inDebateRoleBox, backgroundColor.white)

  val speechRow = style(display.flex, flexDirection.row)

  val speechBox = style(
    display.flex,
    flexDirection.column,
    flexGrow(1),
    margin(mainHalfPadding),
    padding(mainHalfPadding * 2),
    borderRadius(commonBorderRadius)
  )

  val speechHeader = style(fontWeight.bold)

  val speechTimestamp = style(fontStyle.italic)

  val quoteText = style(backgroundColor.yellow, color.black)

  val quoteCitation = style(
    color(c"#aaa")
    // fontStyle.italic
  )

  val debatersList = style()

  val roomMainRow = style(
    flexGrow(1),
    display.flex,
    flexDirection.row
    // padding(mainHalfPadding)
  )

  // val userInfoRow = style(
  //   // flexGrow(1),
  //   width(100 %%),
  //   margin(mainHalfPadding)
  //   // padding(mainHalfPadding)
  // )

  val mainLabeledInputRow = style(
    addClassNames("row"),
    // spaceyContainer,
    padding(1 rem, `0`),
    borderRadius(commonBorderRadius),
    backgroundColor(c"#eee")
  )

  val inputRowLabel = style(
    addClassNames("col-md-2 col-form-label"),
    media.minWidth(768 px)(textAlign.right)
  )

  val inputRowContents = style(
    addClassNames("col-md-10"),
    display.flex,
    flexDirection.column,
    width(100 %%)
  )

  val inputRowItem = style(margin.auto)

  val genericProbSliderLabel     = style(width(14 rem))
  val genericProbSliderProbLabel = style(width(3 rem))
  val genericProbSlider          = style(flexGrow(1))

  val probSlider =
    styleF.int(0 to 4)(i =>
      styleS(
        flexGrow(1),
        accentColor(answerColors(i))
        // &.attr("type", "range").&(Pseudo.Custom("::-webkit-slider-runnable-track", PseudoType.Element))(
        //   answerBg(i)
        // )
      )
    // styleS(
    //   width(75 %%),
    //   if(i % 2 == 0) alignSelf.start else alignSelf.end
    // )
    )

  val answerProbLabel =
    styleF.int(0 to 4)(i =>
      styleS(
        color(answerColors(i)),
        fontSize(1.25 rem),
        fontWeight.bold,
        minWidth(6 rem),
        textAlign.right
      )
    // styleS(
    //   width(75 %%),
    //   if(i % 2 == 0) alignSelf.start else alignSelf.end
    // )
    )

  val correctAnswerLabel = style(color(niceGreen), fontWeight.bold)

  val correctAnswerRadio = style(marginLeft(0.5 em), marginRight(0.5 em))

  val inputLabelDeleteButton = style(color(c"#888"), &.hover(fontWeight.bold, color.black))

  val tentativeInputLabel = style(inputLabel, color(c"#888"))

  val fullWidthInput = style(minHeight(2 rem), width(100 %%), margin(mainHalfPadding))

  val debatePanel = style(
    display.flex,
    flexDirection.row,
    // height(100 %%),
    textAlign.left,
    overflow.hidden
  )

  val debateSubpanel = style(
    flexBasis := "0",
    flexGrow(1),
    display.flex,
    flexDirection.column,
    backgroundColor.white,
    borderRadius(commonBorderRadius),
    // margin(mainHalfPadding),
    position.relative,
    overflow.hidden
  )

  val sourceMaterialSubpanel = style(
    padding(mainHalfPadding),
    width(100 %%),
    height.auto,
    overflowY.auto,
    overflowAnchor.none
  )

  val feedbackSurveySubpanel = style(
    spaceyContainer,
    width(100 %%),
    height.auto,
    overflowY.auto,
    overflowAnchor.none
  )

  val bottomOfDivButton = style(
    addClassNames("btn", "btn-block"),
    borderTopLeftRadius.`0`,
    borderTopRightRadius.`0`
  )

  val comparativeLikertRow = style(
    row,
    addClassNames("flex-wrap", "align-items-center"),
    gap(1 rem)
  )
  val comparativeLikertLabel  = style(minWidth(5 rem))
  val minMaxLikertLabel       = style(addClassNames("small mx-1"), width(6 rem))
  val minMaxLikertLabelBigger = style(addClassNames("small mx-1"), width(7 rem))

  val numberedLikertButton = style(col, grow, addClassNames("small"), maxWidth(4 rem))

  val speechesSubpanel = style(
    flexGrow(1),
    display.flex,
    flexDirection.column,
    height.auto,
    overflowY.auto
    // justifyContent.end
  )

  val speechInputPanel = style()

  val roomBottomRow = style(height(15 rem))

  val speechLengthPanel        = style()
  val speechLengthPanelOverage = style(color.red)

  val correctColor   = green
  val incorrectColor = crimson

  val correct   = style(color(correctColor))
  val incorrect = style(color(incorrectColor))

  val correctBg   = style(backgroundColor(correctColor), color(white))
  val incorrectBg = style(backgroundColor(incorrectColor), color(white))

  val timestampFooter = style(
    marginTop(-.75 rem),
    padding(0 rem, .75 rem, .75 rem),
    textAlign.right
  )

  val speechContent = style(flexGrow(1))

  val speechFooter = style(
    addClassNames("mt-1"),
    color(c"#aaa"),
    padding(0 rem, .25 rem, .25 rem),
    textAlign.right
  )

  @nowarn
  val cardLeftSideXColumn = style(
    col,
    addClassNames("col-auto"),
    alignItems.center,
    textAlign.center,
    borderRight := "1px solid rgba(0,0,0,.125)" // TODO send to bottom with media queries
  )

  val judgeBox       = style(accentColor(green))
  val debaterBox     = style(accentColor(red))
  val disabledSlider = style(accentColor(grey))

  val globalDialogOverlay = style(
    position.fixed,
    left.`0`,
    top.`0`,
    width(100 %%),
    height(100 %%),
    backgroundColor(rgba(0, 0, 0, 0.20))
  )

  val popupDialog = style(margin.auto, marginTop(8 rem), maxWidth(30 rem))
}
