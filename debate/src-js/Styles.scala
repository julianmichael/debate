package debate

import scalacss.DevDefaults._
import scala.language.postfixOps
import scalacss.internal.ValueT

// body{
//   -webkit-touch-callout: none;
//   -webkit-user-select: none;
//   -khtml-user-select: none;
//   -moz-user-select: none;
//   -ms-user-select: none;
//   user-select: none;
// }

/** Stylesheet for the webapp. Inherits default styles for the default view
  * components; these can be overridden.
  */
object Styles extends jjm.ui.View.Styles {
  import dsl._

  val niceGreen = c"#28a745"

  val commonBorderRadius = 5 px
  val mainHalfPadding = 3 px
  val hoverBgColor = c"#eee"

  val customSelect = style(
    addClassNames("custom-select"),
    width.auto,
    flexGrow(1)
  )

  // overrides

  override val textFieldSpan = style(
    display.flex,
    flexDirection.row,
    flexGrow(1)
    // width(100 %%)
  )

  val inputLabel = style(
    addClassNames("col-form-label"),
    width(140 px),
    textAlign.right,
    marginRight(mainHalfPadding)
      // marginRight(5 px),
      // flexGrow(1),
  )

  override val textFieldLabel = inputLabel

  override val textFieldInput = style(
    addClassNames("form-control")
  )

  override val intArrowFieldInput = style(
    width(60 px)
  )

  // list config

  val listConfigListDiv = style(
    textAlign.left
  )
  val listConfigRemoveItemSpan = style(
    color.gray,
    &.hover(
      color.black,
      fontWeight.bold
    )
  )
  val listConfigAddItemDiv = style(
      // textAlign.left
    )
  val listConfigAddItemSpan = listConfigRemoveItemSpan

  // sum config

  val sumConfigOuterDiv = style()
  val sumConfigInnerDiv = style()
  val sumConfigSelect = customSelect
  // style(
  //   addClassNames("custom-select")
  // )

  // prob sliders

  val probSlidersDiv = style()

  // overall styles

  def quotedSeparatedStrings(xs: String*) = {
    xs.map(x => "\"" + x + "\"").mkString(", ")
  }

  style(
    unsafeRoot("body")(
      margin(0 px),
      fontFamily :=! quotedSeparatedStrings(
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
      // fontSize(12 pt)
      // -webkit-font-smoothing: antialiased;
      // -moz-osx-font-smoothing: grayscale;
    )
  )

  style(
    unsafeRoot("code")(
      fontFamily :=! quotedSeparatedStrings(
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

  val adminOnly = style(
    display.none
  )

  // generic arrangement styles

  val row = style(
    display.flex,
    flexDirection.row
  )

  val col = style(
    display.flex,
    flexDirection.column
  )

  val grow = style(
    flexGrow(1)
  )

  val hidden = style(
    visibility.hidden
  )

  // generic coloring/appearance styles

  val simpleUnselectable = style(
    opacity(0.5)
  )

  val simpleSelectable = style(
    &.hover(
      filter := "brightness(85%)"
    )
  )

  val simpleSelected = style(
    backgroundColor(c"#ddd")
  )

  val spaceyPadding = 6 px
  val spaceyContainer = style(
    padding(spaceyPadding / 2),
    unsafeChild("> *") {
      margin(spaceyPadding)
    }
  )
  val spaceySubcontainer = style(
    margin(0 em),
    unsafeChild("> *") {
      margin(spaceyPadding)
    }
  )

  import scalacss.internal.Attr
  // object rowGap extends TypedAttrT1[Len] with ZeroLit {
  //   override val attr = Attr.real("row-gap")
  // }
  // object gridGap extends TypedAttrT1[Len] with ZeroLit {
  //   override val attr = Attr.real("grid-gap")
  // }
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

  // val connectDialog = style(
  //   position.relative,
  //   width(640 px),
  //   height.auto,
  //   margin.auto,
  //   paddingTop(40 px),
  //   display.flex,
  //   flexDirection.column,
  //   unsafeChild("div")(
  //     margin(10 px)
  //   )
  // )

  val settingUpStatusLabel = style(
    color.gold
  )
  val inProgressStatusLabel = style(
    color.darkorange
  )
  val completeStatusLabel = style(
    color.darkgreen
  )

  val loading = style(
    gridRow := "1/-1",
    gridColumn := "1/-1",
    display.flex,
    justifyContent.center,
    alignItems.center
  )

  val lobbyContainer = style(
    addClassNames("container")
  )

  val debateContainer = style(
    addClassNames("container-fluid"),
    height(100 %%),
    display.flex,
    flexDirection.column
  )

  val userInfoMessage = style(
    margin.auto
  )

  val roomRolesRow = style(
    // flexGrow(1),
    width(100 %%),
    display.flex,
    flexDirection.row
    // padding(mainHalfPadding),
  )

  val optionBox = style(
    flexBasis := "0",
    flexGrow(1),
    minHeight(60 px),
    // margin(mainHalfPadding),
    padding(2 px),
    borderStyle.solid,
    borderWidth(1 px),
    borderColor(c"#ddd"),
    borderRadius(commonBorderRadius),
  )

  val optionTitle = style(
    margin(2 px),
    fontWeight.bold
  )

  val judgeColor = darkgreen
  val judgeColorLight = green

  val answerColors = Vector(
      darkblue,
      darkred,
      rebeccapurple,
      darkorange,
      darkturquoise
    )

  val answerColorsLight = Vector(
    lightblue,
    lightpink,
    lightsteelblue,
    lightgoldenrodyellow,
    lightseagreen
  )

  val debateWidthOffset = styleF.int(0 to 4)(i =>
    styleS(
      width(75 %%),
      if (i % 2 == 0) alignSelf.start else alignSelf.end
    )
  )

  val pendingBg = style(
    backgroundColor.coral
  )

  val judgeFeedbackBg = style(
      backgroundColor(judgeColor),
      color.white
    )
  val answerBg = styleF.int(0 to 4)(i =>
    styleS(
      backgroundColor(answerColors(i)),
      color.white
    )
  )

  val noRoleBg = style()
  val facilitatorBg = style(
    backgroundColor.aliceblue,
    color.black
  )
  val observerBg = style(
    backgroundColor(c"#eee"),
    color.black
  )
  val judgeBg = style(
    backgroundColor(judgeColorLight),
    color.white
  )

  val judgeDecision = style(
    borderStyle.solid,
    borderWidth(2 px),
    borderColor.red
  )
  val debaterBg = styleF.int(0 to 4)(i =>
    styleS(
      backgroundColor(answerColorsLight(i)),
      color.white
    )
  )

  val answerOutline = styleF.int(0 to 4)(i =>
    styleS(
      borderStyle.solid,
      borderColor(answerColors(i)),
      color(answerColors(i))
    )
  )

  val noRoleOutline = style()
  val facilitatorOutline = style(
    borderStyle.solid,
    borderColor.aliceblue,
    color.black
  )
  val observerOutline = style(
    borderStyle.solid,
    borderColor(c"#eee"),
    color.black
  )
  val judgeOutline = style(
    borderStyle.solid,
    borderColor.darkgreen,
    color.darkgreen
  )

  val debateColumn = style(
      // position.relative,
      // margin.auto,
      padding(10 px),
      // width(100 %%),
      // height(100 %%),
      display.flex,
      flexDirection.column,
      borderRadius(commonBorderRadius),
      overflow.hidden
    )

  val facilitatorColumn = style(
      spaceySubcontainer,
      // position.relative,
      // margin.auto,
      // padding(10 px),
      // width(100 %%),
      // height(100 %%),
      display.flex,
      flexDirection.column,
      borderRadius(commonBorderRadius)
    )

  val inDebateRoleBox = style(
    backgroundColor.white,
    // width(100 %%),
    fontSize(14 pt),
    textAlign.left,
    borderRadius(commonBorderRadius),
    // margin(mainHalfPadding),
    padding(mainHalfPadding)
  )

  val questionBox = style(
    inDebateRoleBox,
    color(judgeColor)
    // flexGrow(1),
    // display.flex,
    // flexDirection.row,
  )
  val questionBoxCurrent = style(
    inDebateRoleBox,
    color.white,
    backgroundColor(judgeColor)
    // flexGrow(1),
    // display.flex,
    // flexDirection.row,
  )

  val questionTitle = style(
  )

  val questionLabel = style(
    fontWeight.bold
  )

  val judgesLabel = style(
    fontWeight.bold
  )

  val judgesList = style()

  val answerBoxesRow = style(
    display.flex,
    flexDirection.row
  )

  val answerBox = styleF.int(0 to 4)(i =>
    styleS(
      flexBasis := "0",
      flexGrow(1),
      inDebateRoleBox,
      color(answerColors(i))
    )
  )

  val missingParticipant = style(
    fontWeight.lighter,
    fontStyle.italic,
  )

  val answerBoxCurrent = styleF.int(0 to 4)(i =>
    styleS(
      width(75 %%),
      flexGrow(1),
      inDebateRoleBox,
      color.white,
      answerBg(i)
    )
  )

  val answerTitle = style(
    fontWeight.bold
  )

  val speechRow = style(
    display.flex,
    flexDirection.row
  )

  val speechBox = style(
    flexGrow(1),
    margin(mainHalfPadding),
    padding(mainHalfPadding * 2),
    borderRadius(commonBorderRadius)
  )

  val speechHeader = style(
    fontWeight.bold
  )

  val speechTimestamp = style(
    fontStyle.italic
  )

  val quoteText = style(
    backgroundColor.yellow,
    color.black
  )

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
    row, spaceyContainer,
    borderRadius(commonBorderRadius),
    backgroundColor(c"#eee"),
  )

  val inputRowContents = style(
    display.flex,
    flexDirection.column,
    width(100 %%)
  )

  val inputRowItem = style(
    margin.auto
  )

  val probSlider = styleF.int(0 to 4)(i =>
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

  val answerProbLabel = styleF.int(0 to 4)(i =>
    styleS(
      color(answerColors(i)),
      fontSize(14 pt),
      fontWeight.bold,
      minWidth(80 px),
      textAlign.right
    )
  // styleS(
  //   width(75 %%),
  //   if(i % 2 == 0) alignSelf.start else alignSelf.end
  // )
  )

  val correctAnswerLabel = style(
    color(niceGreen),
    fontWeight.bold
  )

  val answerLabel = style(
    addClassNames("col-form-label"),
    marginRight(mainHalfPadding * 2)
    // width(50 px),
    // textAlign.right
    // margin(mainHalfPadding)
    // flexGrow(1),
  )

  val correctAnswerRadio = style(
    inputRowItem,
    margin.auto.important,
    marginLeft(0.5 em).important,
    marginRight(0.5 em).important
  )

  val inputLabelDeleteButton = style(
    color(c"#888"),
    &.hover(
      fontWeight.bold,
      color.black
    )
  )

  val tentativeInputLabel = style(
    inputLabel,
    color(c"#888")
  )

  val fullWidthInput = style(
    width(100 %%),
    margin(mainHalfPadding)
  )

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
    overflowY.auto
  )

  val speechesSubpanel = style(
    flexGrow(1),
    display.flex,
    flexDirection.column,
    height.auto,
    overflowY.auto
    // justifyContent.end
  )

  val speechInputPanel = style(
  )

  val roomBottomRow = style(
    height(200 px)
  )

  val helpModal = style(
    position.fixed,
    top(50 %%),
    left(50 %%),
    marginLeft(-300 px),
    marginTop(-300 px),
    width(600 px),
    height(600 px),
    padding(20 px),
    zIndex(20),
    overflowY.scroll,
    backgroundColor(grey(240)),
    borderRadius(10 px)
  )

  val helpModalCloseButton = style()

  val speechLengthPanel = style()
  val speechLengthPanelOverage = style(
    color.red
  )

}
