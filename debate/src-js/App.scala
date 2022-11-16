package debate

import annotation.unused

import jjm.implicits._

import scalajs.js
import scalajs.js.typedarray.TypedArrayBuffer

import org.scalajs.dom

import org.scalajs.jquery.jQuery

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.MonocleReact._

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

import monocle.macros._

import cats.implicits._

import boopickle.Default._

import org.scalajs.dom.ext.KeyCode

import jjm.ling.Span
import cats.data.NonEmptyList
import jjm.ui.Rgba
import jjm.ling.ESpan
import jjm.ling
import scala.util.Try

import io.circe.generic.JsonCodec

import java.time.{Instant, ZoneId}

// @js.native
// @JSGlobal("showdown.Converter")
// class Converter extends js.Object {
//   def makeHtml(text: String): String = js.native
// }

/** This exists to handle pre-tokenized source material. */
@Lenses @JsonCodec case class DebateSetupRaw(
    rules: DebateRules,
    sourceMaterial: String,
    question: String,
    answers: Vector[String]
)
object DebateSetupRaw {
  def init = DebateSetupRaw(
    rules = DebateRules.default,
    sourceMaterial = "Source material.",
    question = "Question?",
    answers = Vector("Answer 1", "Answer 2")
  )
}

/** The main webapp. */
object App {

  /** Render a list of tokens into HTML, highlighting subspans of that list with
    * various colors.
    *
    * @param tokens
    * @param highlights
    * @param getRenderer
    * @return
    *   a VdomArray of token spans
    */
  def renderHighlightedTokens(
      tokens: Vector[String],
      highlights: Vector[(Span, Rgba)],
      getRenderer: Int => (VdomTag => VdomTag) = Map()
  ) = {
    case class ColorLayering(rgba: Rgba, add: Boolean) // false == remove

    val colorLayerings = highlights
      .flatMap { case (span, color) =>
        Vector(
          span.begin -> ColorLayering(color, true),
          span.endExclusive -> ColorLayering(color, false)
        )
      }
      .sortBy(_._1) :+ (tokens.length -> ColorLayering(
      Rgba.transparent,
      true
    )) // final fake color stack to get it to render out the full content
    case class Acc(
        remainingTokens: Vector[String],
        curOffset: Int,
        colorStack: Vector[Rgba],
        spans: Vector[VdomTag]
    )
    colorLayerings
      .foldLeft(Acc(tokens, 0, Vector(), Vector())) {
        case (
              Acc(text, offset, colors, spans),
              (index, ColorLayering(color, shouldAdd))
            ) =>
          val nextColorSet =
            if (shouldAdd) colors :+ color
            else colors.remove(colors.indexOf(color))
          require(index >= offset) // should never fail bc of sorting property
          if (index == offset) {
            Acc(text, offset, nextColorSet, spans)
          } else {
            val nextIndex = index - offset
            val colorStr = NonEmptyList(Rgba.transparent, colors.toList)
              .reduce((x: Rgba, y: Rgba) => x add y)
              .toColorStyleString
            val endingColorStr =
              if (shouldAdd) colorStr
              else {
                NonEmptyList(Rgba.transparent, nextColorSet.toList)
                  .reduce((x: Rgba, y: Rgba) => x add y)
                  .toColorStyleString
              }
            Acc(
              text.drop(nextIndex),
              index,
              nextColorSet,
              spans ++ text.take(nextIndex).zipWithIndex.flatMap {
                case (token, i) =>
                  val nextSpaceColorStr =
                    if (i == nextIndex - 1) endingColorStr else colorStr
                  if (token == "\n")
                    Vector(<.br(^.key := s"word-${i + offset}"))
                  else
                    Vector[Option[VdomTag]](
                      Option(
                        getRenderer(i + offset)(
                          <.span(
                            ^.key := s"word-${i + offset}",
                            ^.style := js.Dynamic
                              .literal("backgroundColor" -> colorStr),
                            token
                          )
                        )
                      ),
                      Option(
                        <.span(
                          ^.key := s"space-${i + offset}",
                          ^.style := js.Dynamic.literal(
                            "backgroundColor" -> nextSpaceColorStr
                          ),
                          " "
                        )
                      ).filter(_ => i + offset != tokens.size - 1)
                    ).flatten[VdomTag]
              }
            )
          }
      }
      .spans
      .toVdomArray
  }

  val wsProtocol = {
    if (dom.document.location.protocol == "https:") "wss" else "ws"
  }

  def boopickleWebsocket[A: Pickler, B: Pickler] = {
    // import scalajs.js.typedarray._
    import DataConversions._
    new WebSocketConnection2[A, B](
      sendRequest = (socket, req) =>
        Callback {
          // println(s"Sending $req");
          socket.send(Pickle.intoBytes(req))
        },
      readResponse = x => {
        val res = Unpickle[B].fromBytes(TypedArrayBuffer.wrap(x));
        // println(s"Received $res");
        res
      }
    )
  }

  val DebateWebSocket = boopickleWebsocket[DebateState, DebateState]
  def getDebateWebsocketUri(roomName: String, participantId: String): String = {
    s"$wsProtocol://${dom.document.location.host}/ws/$roomName?participantId=$participantId"
  }

  val helpContents = <.div(
    <.h2("Debate")
  )

  import jjm.ui.LocalState

  case class ConnectionSpec(
      roomName: String,
      participantName: String
  )

  val defaultRoomName: String =
    jQuery("#defaultRoomName").attr("value").toOption.getOrElse("")

  // Shortcuts for styles and view elements

  val S = Styles
  val V = new jjm.ui.View(S)

  // instantiate the HOCs we need

  val LocalString = new LocalState[String]
  val LocalDouble = new LocalState[Double]
  val LocalProbs = new LocalState[Vector[Double]]
  val LocalBool = new LocalState[Boolean]
  val LocalStringOpt = new LocalState[Option[String]]
  val LocalConnectionSpecOpt = new LocalState[Option[ConnectionSpec]]
  val LocalSpans = new LocalState[Set[ESpan]]
  val DebateRoomLocal = new LocalState[DebateState]
  val DebateSetupRawLocal = new LocalState[DebateSetupRaw]

  val RoundTypeList =
    ListConfig[DebateRoundType](DebateRoundType.SequentialSpeechesRound(500))
  val RoundTypeConfig = SumConfig[DebateRoundType]()
  val ScoringFunctionConfig = SumConfig[ScoringFunction]()

  val StringField = V.LiveTextField.String
  val StringArea = V.LiveTextArea.String
  val StringOptField = V.LiveTextField[Option[String]](
    x => Some(Option(x).filter(_.nonEmpty)),
    _.getOrElse("")
  )
  val IntOptField = V.LiveTextField[Option[Int]](
    x => if (x.isEmpty) Option(None) else Try(x.toInt).toOption.map(Option(_)),
    // Option(x).flatMap(_.nonEmpty).traverse(i => Try(i.toInt).toOption),
    _.foldMap(_.toString)
  )
  // val TurnTypeSpecLocal = new LocalState[DebateRoundTypeSpec]
  // val TurnTypeSpecSelect = V.Select[DebateRoundTypeSpec](_.toString)

  /** Shows the user's ID. */
  def userInfoRow(name: String, idOpt: Option[ParticipantId]) = {
    <.div(S.userInfoRow)(
      <.span(S.userInfoMessage)(
        "Your name is ",
        <.em(name),
        idOpt.map(_.role) match {
          case None => <.span(" and you have no role!")
          case Some(role) =>
            <.span(
              " and your role is ",
              <.strong(role.toString)
            ) // TODO stylize role
        }
      )
    )
  }

  /** Top row showing non-debating roles (user can click one to change roles).
    */
  def roleChoiceRow(
      roomName: String,
      userName: String,
      debate: DebateState,
      sendState: DebateState => Callback,
      disconnect: Callback
  ) = {

    def assumeRole(role: Role): Callback = {
      sendState(debate.addParticipant(ParticipantId(userName, role)))
    }
    def facilitatorsDiv = {
      val facilitators = debate.participants.collect {
        case ParticipantId(name, Facilitator) => name
      }
      val isCurrent = facilitators.contains(userName)
      val roleBoxStyle =
        if (isCurrent) S.roleBoxCurrent else S.roleBoxNonCurrent
      <.div(roleBoxStyle)(
        <.div(S.roleTitle)("Facilitators"),
        facilitators.toVdomArray(<.span(_)),
        ^.onClick --> assumeRole(Facilitator)
      )
    }
    def observersDiv = {
      val observers = debate.participants.collect {
        case ParticipantId(name, Observer) => name
      }
      val isCurrent = observers.contains(userName)
      val roleBoxStyle =
        if (isCurrent) S.roleBoxCurrent else S.roleBoxNonCurrent
      <.div(roleBoxStyle)(
        <.div(S.roleTitle)("Observers"),
        observers.toVdomArray(<.span(_)),
        ^.onClick --> assumeRole(Observer)
      )
    }
    def downloadButton = <.a(
      ^.href := s"/download/$roomName",
      ^.target := "_blank",
      <.button(S.disconnectButton)("Download")
    )
    def disconnectButton = <.button(S.disconnectButton)(
      "Disconnect",
      ^.onClick --> disconnect
    )

    <.div(S.roomRolesRow)(
      <.div(S.roomRolesRow)(
        facilitatorsDiv,
        observersDiv,
        downloadButton,
        disconnectButton
      )
    )
  }

  /** Config panel for setting a list of round types. */
  def roundTypeSelect(
      roundTypes: StateSnapshot[Vector[DebateRoundType]],
      minItems: Int
  ) = {
    RoundTypeList(roundTypes, minItems) { (remove, roundType, _) =>
      <.div( // (S.labeledInputRow)
        <.span(S.inputRowItem)(remove, " "),
        RoundTypeConfig(roundType)(
          "Simultaneous Speeches" -> SumConfigOption(
            DebateRoundType.SimultaneousSpeechesRound(500),
            DebateRoundType.simultaneousSpeechesRound
          ) { simulSpeeches =>
            <.span(S.inputRowItem)(
              <.span("Character limit"),
              V.NumberField(
                simulSpeeches.zoomStateL(
                  DebateRoundType.SimultaneousSpeechesRound.charLimit
                )
              )
            )
          },
          "Sequential Speeches" -> SumConfigOption(
            DebateRoundType.SequentialSpeechesRound(500),
            DebateRoundType.sequentialSpeechesRound
          ) { seqSpeeches =>
            <.span(S.inputRowItem)(
              <.span("Character limit"),
              V.NumberField(
                seqSpeeches.zoomStateL(
                  DebateRoundType.SequentialSpeechesRound.charLimit
                )
              )
            )
          },
          "Judge Feedback" -> SumConfigOption(
            DebateRoundType.JudgeFeedbackRound(true, 500),
            DebateRoundType.judgeFeedbackRound
          ) { judgeFeedback =>
            <.span(S.inputRowItem)(
              V.Checkbox(
                judgeFeedback.zoomStateL(
                  DebateRoundType.JudgeFeedbackRound.reportBeliefs
                ),
                "Report probabilities"
              ),
              <.span(
                <.span("Character limit"),
                V.NumberField(
                  judgeFeedback.zoomStateL(
                    DebateRoundType.JudgeFeedbackRound.charLimit
                  )
                )
              )
            )
          }
        )
      )
    }
  }

  /** Config panel for facilitator to set the rules of the debate. */
  def facilitatorSetup(
      debate: DebateState,
      sendState: DebateState => Callback
  ) = DebateSetupRawLocal.make(DebateSetupRaw.init) { setupRaw =>
    val answers = setupRaw.value.answers
    <.div(S.debateColumn)(
      <.form(
        ^.onSubmit ==> ((e: ReactEvent) => {
          e.preventDefault();
          val setup = DebateSetup(
            setupRaw.value.rules,
            bigTokenize(setupRaw.value.sourceMaterial),
            setupRaw.value.question,
            setupRaw.value.answers.filter(_.nonEmpty),
            System.currentTimeMillis()
          )
          sendState(
            DebateState.debate
              .composeLens(Debate.setup)
              .set(Some(setup))(debate)
          )
        }),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Opening Rounds"),
          roundTypeSelect(
            setupRaw.zoomStateL(
              DebateSetupRaw.rules.composeLens(DebateRules.fixedOpening)
            ),
            0
          )
        ),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Repeated Rounds"),
          roundTypeSelect(
            setupRaw.zoomStateL(
              DebateSetupRaw.rules.composeLens(DebateRules.repeatingStructure)
            ),
            1
          )
        ),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Judge Scoring Function"),
          ScoringFunctionConfig(
            setupRaw.zoomStateL(
              DebateSetupRaw.rules.composeLens(DebateRules.scoringFunction)
            )
          )(
            "Spherical Score" -> SumConfigOption(
              ScoringFunction.SphericalScoreWithLinearPenalty(3, 1),
              ScoringFunction.sphericalScoreWithLinearPenalty
            ) { sphericalScore =>
              <.span(S.inputRowItem)(
                V.LiveTextField.Double(
                  sphericalScore.zoomStateL(
                    ScoringFunction.SphericalScoreWithLinearPenalty.baseCoefficient
                  ),
                  Some("Base coefficient")
                ),
                V.LiveTextField.Double(
                  sphericalScore.zoomStateL(
                    ScoringFunction.SphericalScoreWithLinearPenalty.perTurnPenalty
                  ),
                  Some("Per turn penalty")
                )
              )
            },
            "Quadratic Score" -> SumConfigOption(
              ScoringFunction.QuadraticScoreWithLinearPenalty(3, 1),
              ScoringFunction.quadraticScoreWithLinearPenalty
            ) { quadraticScore =>
              <.span(S.inputRowItem)(
                V.LiveTextField.Double(
                  quadraticScore.zoomStateL(
                    ScoringFunction.QuadraticScoreWithLinearPenalty.baseCoefficient
                  ),
                  Some("Base coefficient")
                ),
                V.LiveTextField.Double(
                  quadraticScore.zoomStateL(
                    ScoringFunction.QuadraticScoreWithLinearPenalty.perTurnPenalty
                  ),
                  Some("Per turn penalty")
                )
              )
            },
            "Log Score" -> SumConfigOption(
              ScoringFunction.LogScoreWithLinearPenalty(3.0, 1.0, 2.0, 1.0),
              ScoringFunction.logScoreWithLinearPenalty
            ) { logScore =>
              <.span(S.inputRowItem)(
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.baseCoefficient
                  ),
                  Some("Base coefficient")
                ),
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.constant
                  ),
                  Some("Constant")
                ),
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.logBase
                  ),
                  Some("Log base")
                ),
                V.LiveTextField.Double(
                  logScore.zoomStateL(
                    ScoringFunction.LogScoreWithLinearPenalty.perTurnPenalty
                  ),
                  Some("Per turn penalty")
                )
              )
            }
          )
          // roundTypeSelect(setupRaw.zoomStateL(DebateSetupRaw.rules.composeLens(DebateRules.repeatingStructure)), 1),
        ),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Source Material"),
          V.LiveTextArea.String
            .mod(textarea = TagMod(S.fullWidthInput, ^.rows := 10))(
              setupRaw.zoomStateL(DebateSetupRaw.sourceMaterial),
              placeholderOpt = Some("Paste source material here")
            )
        ),
        <.div(S.labeledInputRow)(
          <.div(S.inputLabel)("Question"),
          V.LiveTextField.String.mod(input = S.fullWidthInput)(
            setupRaw.zoomStateL(DebateSetupRaw.question)
          )
        ),
        <.div(S.labeledInputRow)(
          <.span(S.inputLabel)("Answers"),
          ListConfig.String(setupRaw.zoomStateL(DebateSetupRaw.answers), 1) {
            (remove, answer, index) =>
              <.div(S.labeledInputRow)(
                <.span(S.answerLabel)(remove, " ", s"${answerLetter(index)}. "),
                V.LiveTextField.String.mod(input = S.fullWidthInput)(answer)
              )
          }
        ),
        <.button(
          "Start the debate!",
          ^.`type` := "submit",
          ^.disabled := answers.filter(_.nonEmpty).size < 2
        )
      )
    )
  }

  val curHighlightColor = Rgba(255, 255, 0, 0.8)
  val midHighlightColor = Rgba(255, 128, 0, 0.8)
  val spanColorsByDebaterIndex = Map(
    0 -> Rgba(0, 0, 139, 0.4),
    1 -> Rgba(139, 0, 0, 0.4),
    2 -> Rgba(102, 51, 153, 0.4),
    3 -> Rgba(255, 140, 0, 0.4),
    4 -> Rgba(0, 206, 209, 0.4)
  )
  def getSpanColorForRole(role: Role) = role match {
    case Observer       => Rgba(0, 0, 0, 0.6)
    case Facilitator    => Rgba(285, 293, 200, 0.5)
    case Judge          => Rgba(0, 100, 0, 0.4)
    case Debater(index) => spanColorsByDebaterIndex(index)
  }

  /** Show whose turn it is. */
  def turnDisplay(
      roleOpt: Option[Role],
      turnOpt: Option[DebateTurnType]
  ) = <.div(
    turnOpt match {
      case None => <.span("The debate is over.")
      case Some(turn) =>
        turn match {
          case DebateTurnType.SimultaneousSpeechesTurn(remainingDebaters, _) =>
            roleOpt match {
              case Some(Debater(index)) =>
                if (remainingDebaters.contains(index)) {
                  <.span(
                    "It is YOUR TURN. All debaters are constructing simultaneous speeches."
                  )
                } else {
                  <.span(
                    "Your speech has been received. Waiting for other debaters."
                  )
                }
              case _ =>
                <.span("All debaters are constructing simultaneous speeches.")
            }
          case DebateTurnType.DebaterSpeechTurn(index, _) =>
            roleOpt match {
              case Some(Debater(`index`)) =>
                <.span("It is YOUR TURN to make a speech.")
              case _ => <.span(s"It is Debater ${answerLetter(index)}'s turn.")
            }
          case DebateTurnType.JudgeFeedbackTurn(_, _) =>
            roleOpt match {
              case Some(Judge) =>
                <.span("It is YOUR TURN as judge to give feedback.")
              case _ => <.span(s"It is the Judge's turn to give feedback.")
            }
        }
    }
  )

  /** Show the debate. */
  def debatePanel(
      userId: Option[ParticipantId],
      setup: DebateSetup,
      debate: Debate,
      sendDebate: Debate => Callback
  ) = {
    val turns = debate.turns
    val role = userId.map(_.role)
    val shouldShowSourceMaterial = role match {
      case Some(Facilitator | Debater(_)) => true
      case _                              => false
    }
    val inProgressSpeechStyle = role match {
      case None              => TagMod(S.noRoleOutline)
      case Some(Facilitator) => TagMod(S.facilitatorOutline)
      case Some(Observer)    => TagMod(S.observerOutline)
      case Some(Judge)       => TagMod(S.judgeOutline)
      case Some(Debater(index)) =>
        TagMod(S.answerOutline(index), S.debateWidthOffset(index))
    }

    val currentTurnOpt = debate.currentTurn
    val isUsersTurn = currentTurnOpt.exists {
      case DebateTurnType.SimultaneousSpeechesTurn(_, _) =>
        role.collect { case Debater(_) => () }.nonEmpty
      case DebateTurnType.DebaterSpeechTurn(curSpeaker, _) =>
        role.collect { case Debater(`curSpeaker`) => () }.nonEmpty
      case DebateTurnType.JudgeFeedbackTurn(_, _) =>
        role.collect { case Judge => () }.nonEmpty
    }
    val charLimit = currentTurnOpt.fold(-1)(_.charLimit)

    isUsersTurn && (
      role match {
        case None | Some(Observer) => false
        case _                     => true
      }
    )

    def makeSimpleVdomFromText(text: String) = {
      <.span(
        text
          .split("\n")
          .toVector
          .map(x => Vector(<.span(x): VdomElement))
          .intercalate(Vector(<.br()))
          .toVdomArray
      )
    }

    def breakNewlines(x: String) = {
      x.split("\n")
        .toVector
        .map(seg => Vector[VdomNode](<.span(seg)))
        .intercalate(Vector(<.br()))
        .toVdomArray
    }

    def minSecTime(millis: Long): String = {
      val secs = millis / 1000
      val mins = secs / 60
      val secsRem = secs % 60
      s"${mins}m ${secsRem}s"
    }

    def speechToHTML(speech: DebateSpeech) = {
      val roleString = speech.speaker.role.toString
      <.div(S.speechHeader)(
        speech.speaker.name,
        s" ($roleString) ",
        <.span(S.speechTimestamp)(
          minSecTime(speech.timestamp - setup.startTime),
          " into the debate at ", {
            val base = {
              Instant
                .ofEpochMilli(speech.timestamp)
                // TODO this should perhaps display it in the client's timezone
                .atZone(
                  ZoneId.of("Z")
                ) // see "time zones" on http://cquiroz.github.io/scala-java-time/
                .toLocalTime
                .toString
            }
            base + " UTC"
          }
        ).when(speech.timestamp > 0)
      )
    }

    def quoteToHTML(span: ESpan) = {
      <.span(
        <.span(S.quoteText)(
          breakNewlines(ling.Text.renderSpan(setup.sourceMaterial, span))
        ),
        <.span(S.quoteCitation)(s" (${span.begin}–${span.end})")
      )
    }

    def makeSimultaneousSpeechesHtml(
        speeches: Map[Int, DebateSpeech],
        speechIndex: Int
    ) = {
      <.div(S.speechRow)(
        ^.key := s"speech-$speechIndex",
        speeches.toVector.sortBy(_._1).toVdomArray {
          case (debaterIndex, speech) =>
            <.div(S.speechBox, S.answerBg(debaterIndex))(
              ^.key := s"speech-$speechIndex-$debaterIndex",
              speechToHTML(speech),
              speech.content.toVdomArray {
                case SpeechSegment.Text(text)  => makeSimpleVdomFromText(text)
                case SpeechSegment.Quote(span) => quoteToHTML(span)
              }
            )
        }
      )
    }

    def makeSpeechHtml(
        speech: DebateSpeech,
        style: TagMod,
        speechIndex: Int
    ) = {
      <.div(S.speechBox, style)(
        ^.key := s"speech-$speechIndex",
        speechToHTML(speech),
        speech.content.toVdomArray {
          case SpeechSegment.Text(text)  => makeSimpleVdomFromText(text)
          case SpeechSegment.Quote(span) => quoteToHTML(span)
        }
      )
    }
    def getSpeechLength(speechSegments: Vector[SpeechSegment]) = {
      speechSegments.foldMap {
        case SpeechSegment.Text(text) => text.size
        case SpeechSegment.Quote(span) =>
          ling.Text.renderSpan(setup.sourceMaterial, span).size
      }
    }

    val scrollDebateToBottom = Callback {
      val newSpeechesJQ = jQuery("#speeches")
      val newSpeechesDiv = newSpeechesJQ(0)
      newSpeechesJQ.scrollTop(
        newSpeechesDiv.scrollHeight - newSpeechesDiv.clientHeight
      )
    }
    def maybeScrollDebateToBottom = {
      val speechesJQ = jQuery("#speeches")
      val speechesDiv = speechesJQ(0)
      val isScrolledToBottom =
        speechesDiv.scrollHeight - speechesJQ.scrollTop() - speechesJQ
          .outerHeight() < 1
      if (isScrolledToBottom) scrollDebateToBottom else Callback.empty
    }

    val debateSpansWithSpeaker = turns.flatMap {
      case SimultaneousSpeeches(speeches) =>
        speeches.values.toVector.flatMap(speech =>
          speech.allQuotes.map(speech.speaker -> _)
        )
      case DebaterSpeech(speech) =>
        speech.allQuotes.map(speech.speaker -> _)
      case JudgeFeedback(_, _) => Vector()
    }

    LocalSpans.make(Set.empty[ESpan]) { curMessageSpans =>
      val highlights = debateSpansWithSpeaker.map { case (id, span) =>
        span -> getSpanColorForRole(id.role)
      } ++ curMessageSpans.value.toVector.map(_ -> curHighlightColor)

      <.div(S.debatePanel)(
        <.div(S.debateSubpanel)(
          SpanSelection2.make(
            true,
            (ispan => curMessageSpans.modState(_ + ispan.toExclusive))
          ) { case (status, context) =>
            val selectingSpanColorOpt =
              SpanSelection2.Status.selecting.getOption(status).map {
                case SpanSelection2.Selecting(begin, end) =>
                  ESpan(begin, end + 1) -> midHighlightColor
              }
            val allHighlights = highlights ++ selectingSpanColorOpt

            <.div(S.sourceMaterialSubpanel)(
              renderHighlightedTokens(
                setup.sourceMaterial,
                allHighlights,
                setup.sourceMaterial.indices.toVector
                  .map(i =>
                    i -> ((el: VdomTag) =>
                      el(
                        ^.onMouseMove --> context.hover(i),
                        ^.onClick --> context.touch(i)
                      )
                    )
                  )
                  .toMap
              )
            )
          }
        ).when(shouldShowSourceMaterial),
        LocalQuotingMessage.make(
          curMessageSpans,
          "",
          didUpdate = _ => scrollDebateToBottom
        ) { currentMessage =>
          val currentMessageSpeechSegments =
            SpeechSegment.getSegmentsFromString(currentMessage.value)

          val speechLength = getSpeechLength(currentMessageSpeechSegments)
          val speechIsTooLong = charLimit > 0 && speechLength > charLimit

          def speechInputPanel(
              submit: Callback,
              cmdEnterToSubmit: Boolean = true
          ) = {
            <.div(S.speechInputPanel)(
              V.LiveTextArea.String.mod(
                textarea = TagMod(
                  S.fullWidthInput,
                  ^.rows := 5,
                  ^.onKeyDown ==> ((e: ReactKeyboardEvent) => {
                    val submitCB =
                      if (
                        cmdEnterToSubmit &&
                        e.keyCode == KeyCode.Enter && (e.metaKey || e.ctrlKey)
                      ) {
                        submit
                      } else Callback.empty
                    submitCB
                  })
                )
              )(
                currentMessage,
                didUpdateValue = _ => maybeScrollDebateToBottom
              ),
              <.div(
                S.speechLengthPanel,
                S.invalidTextBackground.when(speechIsTooLong)
              )(
                "Length: ",
                <.span(S.speechLengthPanelOverage.when(speechIsTooLong))(
                  speechLength.toString
                ),
                <.span(" / ", charLimit.toString).when(charLimit > 0)
              )
            )
          }

          <.div(S.debateSubpanel)(
            <.div(S.speechesSubpanel)(
              ^.id := "speeches",
              turns.zipWithIndex.flatMap { case (turn, speechIndex) =>
                turn match {
                  case SimultaneousSpeeches(speeches) =>
                    if (speeches.size < setup.answers.size) {
                      role
                        .collect { case Debater(index) =>
                          speeches.get(index).map { speech =>
                            val speechStyle = TagMod(
                              S.answerOutline(index),
                              S.pendingBg,
                              S.debateWidthOffset(index)
                            )
                            makeSpeechHtml(speech, speechStyle, speechIndex)
                          }
                        }
                        .flatten
                        .toVector
                    } else {
                      Vector(
                        makeSimultaneousSpeechesHtml(speeches, speechIndex)
                      )
                    }
                  case DebaterSpeech(speech) =>
                    val speechStyle = speech.speaker.role match {
                      case Facilitator => TagMod(S.facilitatorBg)
                      case Observer    => TagMod(S.observerBg)
                      case Judge       => TagMod(S.judgeBg)
                      case Debater(index) =>
                        TagMod(S.answerBg(index), S.debateWidthOffset(index))
                    }
                    Vector(makeSpeechHtml(speech, speechStyle, speechIndex))
                  case JudgeFeedback(probabilities, speech) =>
                    val speechStyle = speech.speaker.role match {
                      case Facilitator => TagMod(S.facilitatorBg)
                      case Observer    => TagMod(S.observerBg)
                      case Judge       => TagMod(S.judgeBg)
                      case Debater(index) =>
                        TagMod(S.answerBg(index), S.debateWidthOffset(index))
                    }
                    Vector(
                      Option(makeSpeechHtml(speech, speechStyle, speechIndex)),
                      Option(
                        <.div(
                          ^.display := "flex",
                          ^.flexDirection := "row",
                          probabilities.zipWithIndex.toVdomArray {
                            case (prob, index) =>
                              val pct = f"${prob * 100.0}%.0f%%"
                              <.div(
                                S.answerBg(index),
                                ^.width := pct,
                                ^.color := "white",
                                ^.fontWeight := "bold",
                                ^.flexGrow := "1"
                              )(pct)
                          }
                        )
                      ).filter(_ => probabilities.size > 1)
                    ).flatten
                }
              }.toVdomArray,
              userId.whenDefined { userId =>
                makeSpeechHtml(
                  DebateSpeech(userId, -1L, currentMessageSpeechSegments),
                  inProgressSpeechStyle,
                  -1
                ).when(currentMessage.value.size > 0)
              }
            ),
            turnDisplay(role, currentTurnOpt),
            currentTurnOpt.filter(_ => isUsersTurn).whenDefined {
              case DebateTurnType.SimultaneousSpeechesTurn(
                    _: Set[Int],
                    _: Int
                  ) =>
                val submit =
                  (
                    if (!isUsersTurn || speechIsTooLong) Callback.empty
                    else
                      userId.foldMap(userId =>
                        CallbackTo(System.currentTimeMillis()).flatMap { time =>
                          val speech = DebateSpeech(
                            userId,
                            time,
                            currentMessageSpeechSegments
                          )
                          val newTurns = role match {
                            case Some(Debater(debaterIndex)) =>
                              turns.lastOption match {
                                // If we're in the middle of a simultaneous speech round that hasn't finished yet, add/update our speech to the turn
                                case Some(SimultaneousSpeeches(speeches))
                                    if speeches.size < setup.answers.size =>
                                  turns.updated(
                                    turns.size - 1,
                                    SimultaneousSpeeches(
                                      speeches + (debaterIndex -> speech)
                                    )
                                  )
                                // otherwise, create the record for the turn with our new speech.
                                case _ =>
                                  turns :+ SimultaneousSpeeches(
                                    Map(debaterIndex -> speech)
                                  )
                              }
                            case _ => turns
                          }
                          sendDebate(Debate(Some(setup), newTurns))
                        } >> currentMessage.setState("")
                      )
                  )

                <.div(S.col)(
                  speechInputPanel(submit),
                  <.button(
                    "Submit",
                    ^.disabled := !isUsersTurn || speechIsTooLong,
                    ^.onClick --> submit
                  )
                )

              case DebateTurnType
                    .DebaterSpeechTurn(_: Int, _: Int) =>
                val submit =
                  (
                    if (!isUsersTurn || speechIsTooLong) Callback.empty
                    else
                      userId.foldMap(userId =>
                        CallbackTo(System.currentTimeMillis()).flatMap(time =>
                          sendDebate(
                            Debate(
                              Some(setup),
                              turns :+ DebaterSpeech(
                                DebateSpeech(
                                  userId,
                                  time,
                                  currentMessageSpeechSegments
                                )
                              )
                            )
                          )
                        ) >> currentMessage.setState("")
                      )
                  )

                <.div(S.col)(
                  speechInputPanel(submit),
                  <.button(
                    "Submit",
                    ^.disabled := !isUsersTurn || speechIsTooLong,
                    ^.onClick --> submit
                  )
                )

              case DebateTurnType.JudgeFeedbackTurn(
                    _: Boolean,
                    _: Int
                  ) =>
                val turnNum = debate.turns.collect { case JudgeFeedback(_, _) =>
                  1
                }.sum
                LocalProbs.make(
                  Vector.fill(setup.answers.size)(1.0 / setup.answers.size)
                ) { probs =>
                  val submit =
                    (
                      if (!isUsersTurn || speechIsTooLong) Callback.empty
                      else
                        userId.foldMap(userId =>
                          CallbackTo(System.currentTimeMillis()).flatMap(time =>
                            sendDebate(
                              Debate(
                                Some(setup),
                                turns :+ JudgeFeedback(
                                  probs.value,
                                  DebateSpeech(
                                    userId,
                                    time,
                                    Vector(
                                      SpeechSegment.Text(currentMessage.value)
                                    )
                                  )
                                )
                              )
                            )
                          ) >> currentMessage.setState("")
                        )
                    )

                  LocalBool.make(false) { consideringContinue =>
                    val barWidthPx = 60

                    <.div(S.row)(
                      <.div(S.grow)(
                        ProbabilitySliders(probs) {
                          case ProbabilitySliders
                                .Context(index, prob, setProb) =>
                            <.div(S.row)(
                              <.span(S.answerProbLabel(index))(
                                f"${prob * 100.0}%.0f%% ${answerLetter(index)}. "
                              ),
                              <.input(S.probSlider(index))(
                                ^.`type` := "range",
                                ^.min := 0,
                                ^.max := 1,
                                ^.step := 0.01,
                                ^.value := prob,
                                ^.onChange ==> ((e: ReactEventFromInput) => {
                                  setProb(e.target.value.toDouble)
                                })
                              )
                            )
                        },
                        speechInputPanel(submit, false),
                        <.div(S.row)(
                          <.button(S.grow)(
                            "End debate & collect reward",
                            ^.disabled := !isUsersTurn || speechIsTooLong,
                            ^.onClick --> submit
                          ),
                          <.button(S.grow)(
                            f"Continue debate for $$${setup.rules.scoringFunction.perTurnPenalty}%.2f",
                            ^.disabled := !isUsersTurn || speechIsTooLong,
                            ^.onMouseMove --> consideringContinue
                              .setState(true),
                            ^.onMouseLeave --> consideringContinue
                              .setState(false),
                            ^.onClick --> submit
                          )
                        )
                      ),
                      <.div(
                        S.col,
                        ^.width := s"${barWidthPx * setup.answers.size}px"
                      ) {
                        val currentScores = setup.answers.indices.map(index =>
                          setup.rules.scoringFunction
                            .eval(turnNum, probs.value, index)
                        )
                        val hypotheticalScores =
                          setup.answers.indices.map(index =>
                            setup.rules.scoringFunction
                              .eval(turnNum + 1, probs.value, index)
                          )

                        val reqdDeltas =
                          ScoringFunction.deltasForNextTurnToBeWorthwhile(
                            setup.rules.scoringFunction,
                            probs.value,
                            turnNum
                          )

                        val scores =
                          if (consideringContinue.value) hypotheticalScores
                          else currentScores

                        val maxNegMagnitude = 20.0
                        val min =
                          math.min(0, math.max(-maxNegMagnitude, scores.min))
                        val range = setup.rules.scoringFunction.max - min
                        val maxRelScore =
                          math.abs(setup.rules.scoringFunction.max) / range
                        val maxNegRelScore =
                          math.min(maxNegMagnitude, math.abs(min)) / range
                        TagMod(
                          <.div(S.col, S.grow)(
                            <.div(S.row)(
                              setup.answers.indices.zip(scores).toVdomArray {
                                case (index, _) =>
                                  <.div(S.col, ^.width := s"${barWidthPx}px")(
                                    reqdDeltas(index).whenDefined(delta =>
                                      <.span(f"+${delta * 100}%.0f%%")
                                    )
                                  )
                              }
                            ).when(consideringContinue.value),
                            <.div(
                              S.row,
                              ^.height := f"${maxRelScore * 100}%.2f%%"
                            )(
                              setup.answers.indices.zip(scores).toVdomArray {
                                case (index, score) =>
                                  val relScore = math.abs(score) / range
                                  <.div(S.col, ^.width := s"${barWidthPx}px")(
                                    <.div(
                                      S.col,
                                      S.grow,
                                      ^.justifyContent := "flex-end"
                                    )(
                                      ^.position := "relative",
                                      <.div(f"$$${score}%.2f")
                                        .when(relScore <= 0.10),
                                      <.div(
                                        S.answerBg(index),
                                        ^.height := f"${relScore / maxRelScore * 100}%.2f%%",
                                        <.div(f"$$${score}%.2f")
                                          .when(relScore > 0.10)
                                      )
                                    ).when(score >= 0.0)
                                  )
                              }
                            ),
                            <.div(
                              S.row,
                              ^.height := f"${maxNegRelScore * 100}%.2f%%"
                            )(
                              setup.answers.indices.zip(scores).toVdomArray {
                                case (index, score) =>
                                  val relScore = math.min(
                                    maxNegMagnitude,
                                    math.abs(score)
                                  ) / range
                                  <.div(S.col, ^.width := s"${barWidthPx}px")(
                                    <.div(
                                      S.col,
                                      S.grow,
                                      ^.justifyContent := "flex-start"
                                    )(
                                      ^.position := "relative",
                                      <.div(
                                        S.col,
                                        ^.justifyContent := "flex-end"
                                      )(
                                        S.answerBg(index),
                                        ^.height := f"${relScore / maxNegRelScore * 100}%.2f%%",
                                        <.div(f"$$${score}%.2f")
                                          .when(relScore < -0.10)
                                      ),
                                      <.div(f"$$${score}%.2f")
                                        .when(relScore >= -0.10)
                                    ).when(score < 0.0)
                                  )
                              }
                            )
                          ),
                          <.div(S.row)(
                            setup.answers.indices.toVdomArray { case index =>
                              <.div(
                                S.inputRowItem,
                                ^.width := s"${barWidthPx}px"
                              )(answerLetter(index))
                            }
                          )
                        )
                      }
                    )
                  }
                }
            }
          )
        }
      )
    }
  }

  class Backend(@unused scope: BackendScope[Unit, Unit]) {

    /** Main render method. */
    def render(@unused props: Unit, @unused state: Unit) = {
      <.div(S.app)(
        LocalConnectionSpecOpt.make(None) { connectionSpecOpt =>
          connectionSpecOpt.value match {
            case None =>
              LocalString.make("") { roomNameLive =>
                LocalString.make("") { participantNameLive =>
                  <.div(S.connectDialog)(
                    <.form(
                      ^.onSubmit ==> ((e: ReactEvent) => {
                        val roomName = roomNameLive.value
                        e.preventDefault()
                        connectionSpecOpt.setState(
                          Some(
                            ConnectionSpec(
                              roomName,
                              participantNameLive.value
                            )
                          )
                        )
                        // >> Callback(dom.window.document.title = makePageTitle(roomName)) >>
                        // Callback(dom.window.history.replaceState("", makePageTitle(roomName), roomName))
                        // roomName.setState(roomNameLive.value)
                      }),
                      <.div(
                        "Name: ",
                        V.LiveTextField.String(participantNameLive)
                      ),
                      <.div("Room: ", StringField(roomNameLive)),
                      <.div(
                        <.button(
                          "Join",
                          ^.`type` := "submit",
                          ^.disabled := roomNameLive.value.isEmpty || participantNameLive.value.isEmpty
                        )
                      )
                    )
                  )
                }
              }
            case Some(ConnectionSpec(roomName, userName)) =>
              DebateRoomLocal.make(DebateState.init) { debate =>
                DebateWebSocket.make(
                  getDebateWebsocketUri(roomName, userName),
                  onOpen = _ => Callback(println("Chat socket opened.")),
                  onMessage = (_, msg) => {
                    debate.setState(msg)
                  }
                ) {
                  case DebateWebSocket.Disconnected(_, reason) =>
                    <.div(S.loading)(
                      """You've been disconnected. In all likelihood this is because I'm
                         updating/restarting the server. Please refresh in a minute or two.
                         Sorry about that, lol. Error details:
                      """ + reason
                    )
                  case DebateWebSocket.Connecting =>
                    <.div(S.loading)("Connecting to metadata server...")
                  case DebateWebSocket.Connected(sendState, _) =>
                    val userId =
                      debate.value.participants.find(_.name == userName)
                    val backgroundStyle = S.observerBg

                    <.div(S.roomMainColumn)(
                      roleChoiceRow(
                        roomName,
                        userName,
                        debate.value,
                        sendState,
                        disconnect = connectionSpecOpt.setState(None)
                      ),
                      userInfoRow(userName, userId),
                      debate.value.debate.setup match {
                        case None =>
                          userId.map(_.role) match {
                            case Some(Facilitator) =>
                              facilitatorSetup(debate.value, sendState)
                            case _ =>
                              <.div(S.debateColumn)(
                                "Waiting for a facilitator to set up the debate."
                              )
                          }
                        case Some(setup) =>
                          def assumeRole(role: Role): Callback = {
                            sendState(
                              debate.value.addParticipant(
                                ParticipantId(userName, role)
                              )
                            )
                          }
                          val isCurrentJudge = userId
                            .map(_.role)
                            .collect { case Judge => true }
                            .nonEmpty
                          val questionBoxStyle =
                            if (isCurrentJudge) S.questionBoxCurrent
                            else S.questionBox
                          <.div(S.debateColumn, backgroundStyle)(
                            <.div(questionBoxStyle)(
                              <.div(S.questionTitle)(
                                <.span(S.questionLabel)("Question: "),
                                setup.question
                              ),
                              <.div(S.judgesList)(
                                "Judges: ",
                                debate.value.participants.collect {
                                  case ParticipantId(name, Judge) =>
                                    <.span(name)
                                }.toVdomArray
                              ),
                              ^.onClick --> assumeRole(Judge)
                            ),
                            <.div(S.answerBoxesRow)(
                              setup.answers.zipWithIndex.toVdomArray {
                                case (answer, answerIndex) =>
                                  val isCurrent = userId
                                    .map(_.role)
                                    .collect { case Debater(`answerIndex`) =>
                                      true
                                    }
                                    .nonEmpty
                                  val answerBoxStyle =
                                    if (isCurrent) S.answerBoxCurrent
                                    else S.answerBox
                                  <.div(answerBoxStyle(answerIndex))(
                                    <.div(S.answerTitle)(
                                      s"${answerLetter(answerIndex)}. $answer"
                                    ),
                                    <.div(S.debatersList)(
                                      "Debaters: ",
                                      debate.value.participants.collect {
                                        case ParticipantId(
                                              name,
                                              Debater(`answerIndex`)
                                            ) =>
                                          <.span(name)
                                      }.toVdomArray
                                    ),
                                    ^.onClick --> assumeRole(
                                      Debater(answerIndex)
                                    )
                                  )
                              }
                            ),
                            debatePanel(
                              userId,
                              setup,
                              debate.value.debate,
                              (d: Debate) =>
                                sendState(
                                  DebateState.debate.set(d)(debate.value)
                                )
                            )
                          )
                      }
                    )
                }
              }
          }
        }
      )
    }
  }

  val Component = ScalaComponent
    .builder[Unit]("Full UI")
    .initialState(())
    .renderBackend[Backend]
    .build

  def setupUI(): Unit = {
    Styles.addToDocument()
    Component().renderIntoDOM(
      org.scalajs.dom.document.getElementById("contents")
    )
  }

  final def main(args: Array[String]): Unit = jQuery { () =>
    setupUI()
  }
}
