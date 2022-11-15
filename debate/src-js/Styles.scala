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

  // overrides

  override val textFieldSpan = style(
    flexGrow(1)
    // width(100 %%)
  )

  override val intArrowFieldInput = style(
    width(60 px)
  )

  // list config

  val listConfigListDiv = style(
    textAlign.left
  )
  val listConfigItemDiv = style(
    // textAlign.left
  )
  val listConfigRemoveItemSpan = style(
    color.gray,
    &.hover(
      color.black,
      fontWeight.bold
    )
  )
  val listConfigAddItemSpan = listConfigRemoveItemSpan

  // sum config

  val sumConfigOuterSpan = style()
  val sumConfigInnerSpan = style()
  val sumConfigSelect = style()

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
    // display.grid,
    // gridTemplateColumns := "1fr 1fr",
    // gridTemplateRows := "1fr 1fr",
    // gridTemplateAreas := "\"TL TR\" \"B B\"",
    textAlign.center,
    // columnGap(5 px),
    // rowGap(5 px),
    // padding(5 px),
    // boxSizing.borderBox,
    position.absolute,
    top.`0`,
    left.`0`,
    height(100 vh),
    width(100 vw),
    overflowX.hidden
  )

  val connectDialog = style(
    position.relative,
    width(640 px),
    height.auto,
    margin.auto,
    paddingTop(40 px),
    display.flex,
    flexDirection.column,
    unsafeChild("div")(
      margin(10 px)
    )
  )

  val loading = style(
    gridRow := "1/-1",
    gridColumn := "1/-1",
    display.flex,
    justifyContent.center,
    alignItems.center
  )

  val roomMainColumn = style(
    position.relative,
    margin.auto,
    padding(5 px),
    maxWidth(1200 px),
    height(100 vh),
    display.flex,
    flexDirection.column
  )

  val userInfoMessage = style(
    margin.auto
  )

  val commonBorderRadius = 5 px
  val mainHalfPadding = 3 px
  val hoverBgColor = c"#eee"

  val roomRolesRow = style(
    // flexGrow(1),
    width(100 %%),
    display.flex,
    flexDirection.row
    // padding(mainHalfPadding),
  )

  val disconnectButton = style(
    margin(mainHalfPadding),
    padding(2 px)
  )

  val roleBox = style(
    flexGrow(1),
    height(50 px),
    margin(mainHalfPadding),
    padding(2 px),
    borderStyle.solid,
    borderWidth(1 px),
    borderColor(c"#ddd"),
    borderRadius(commonBorderRadius),
    &.hover(
      filter := "brightness(85%)"
    )
  )

  val roleBoxNonCurrent = style(
    roleBox,
    backgroundColor.white
  )

  val roleBoxCurrent = style(
    roleBox,
    backgroundColor(c"#ddd")
  )

  val roleTitle = style(
    margin(2 px),
    fontWeight.bold
  )

  val answerColors = Vector(
    darkblue,
    darkred,
    rebeccapurple,
    darkorange,
    darkturquoise
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
    backgroundColor.green,
    color.white
  )

  val answerOutline = styleF.int(0 to 4)(i =>
    styleS(
      border.solid,
      borderColor(answerColors(i)),
      color(answerColors(i))
    )
  )

  val noRoleOutline = style()
  val facilitatorOutline = style(
    border.solid,
    borderColor.aliceblue,
    color.black
  )
  val observerOutline = style(
    border.solid,
    borderColor(c"#eee"),
    color.black
  )
  val judgeOutline = style(
    border.solid,
    borderColor.darkgreen,
    color.darkgreen
  )

  val debateColumn = style(
    position.relative,
    // margin.auto,
    padding(10 px),
    width(100 %%),
    height(100 %%),
    display.flex,
    flexDirection.column,
    borderRadius(commonBorderRadius),
    overflow.hidden
  )

  val inDebateRoleBox = style(
    backgroundColor.white,
    width(100 %%),
    fontSize(14 pt),
    textAlign.left,
    &.hover(
      filter := "brightness(85%)"
    ),
    borderRadius(commonBorderRadius),
    margin(mainHalfPadding),
    padding(mainHalfPadding)
  )

  val questionBox = style(
    inDebateRoleBox,
    color.darkgreen
    // flexGrow(1),
    // display.flex,
    // flexDirection.row,
  )
  val questionBoxCurrent = style(
    inDebateRoleBox,
    color.white,
    backgroundColor.darkgreen
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
      flexGrow(1),
      inDebateRoleBox,
      color(answerColors(i))
    )
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

  val userInfoRow = style(
    // flexGrow(1),
    width(100 %%),
    margin(mainHalfPadding)
    // padding(mainHalfPadding)
  )

  val labeledInputRow = style(
    // flexGrow(1),
    display.flex,
    flexDirection.row
    // padding(mainHalfPadding)
  )

  val inputLabel = style(
    width(120 px),
    textAlign.right,
    margin(mainHalfPadding)
    // flexGrow(1),
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

  val answerLabel = style(
    inputRowItem
    // width(50 px),
    // textAlign.right
    // margin(mainHalfPadding)
    // flexGrow(1),
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
    margin(mainHalfPadding),
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
