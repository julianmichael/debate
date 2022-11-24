package debate

import annotation.unused

// import jjm.implicits._

// import scalajs.js
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

// import org.scalajs.dom.ext.KeyCode

import scala.util.Try

import io.circe.generic.JsonCodec

// import java.time.{Instant, ZoneId}
import cats.Foldable
import cats.Functor
import debate.MainChannelRequest

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
    answers: Vector[String],
    roles: Map[DebateRole, String],
    correctAnswerIndex: Int
) {
  def areAllRolesAssigned = {
    roles.contains(Judge) && answers.indices.forall(i => roles.contains(Debater(i)))
  }
}
object DebateSetupRaw {
  def init = DebateSetupRaw(
    rules = DebateRules.default,
    sourceMaterial = "Source material.",
    question = "Question?",
    answers = Vector("Answer 1", "Answer 2"),
    roles = Map(),
    correctAnswerIndex = 0
  )
}

/** The main webapp. */
object App {

  def commaSeparatedSpans[F[_]: Foldable: Functor](fa: F[String]) = {
    fa.map(x => Vector(<.span(x))).intercalate(Vector(<.span(", ")))
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
  def getDebateWebsocketUri(isScheduled: Boolean, roomName: String, participantId: String): String = {
    val prefix = if(isScheduled) "scheduled" else "open"
    s"$wsProtocol://${dom.document.location.host}/$prefix-ws/$roomName?name=$participantId"
  }

  val MainWebSocket = boopickleWebsocket[MainChannelRequest, Lobby]
  val mainWebsocketUri: String = {
    s"$wsProtocol://${dom.document.location.host}/main-ws"
  }

  import jjm.ui.LocalState

  case class ConnectionSpec(
    isScheduled: Boolean,
    roomName: String,
    participantName: String
  )


  sealed trait LobbyTab extends Product with Serializable {
    import LobbyTab._
    override def toString = this match {
      case MyCurrentDebates => "My Current Debates"
      case AllMyDebates => "All My Debates"
      case OpenDebates => "Open Debates"
    }
  }
  object LobbyTab {
    case object MyCurrentDebates extends LobbyTab
    case object AllMyDebates extends LobbyTab
    case object OpenDebates extends LobbyTab
  }

  val defaultRoomName: String =
    jQuery("#defaultRoomName").attr("value").toOption.getOrElse("")

  // Shortcuts for styles and view elements

  val S = Styles
  val V = new jjm.ui.View(S)

  // instantiate the HOCs we need

  val LocalLobbyTab = new LocalState[LobbyTab]
  val LocalString = new LocalState[String]
  val LocalDouble = new LocalState[Double]
  val LocalStringOpt = new LocalState[Option[String]]
  val LocalConnectionSpecOpt = new LocalState[Option[ConnectionSpec]]
  val LocalLobby = new LocalState[Lobby]
  val DebateRoomLocal = new LocalState[DebateState]
  val DebateSetupRawLocal = new LocalState[DebateSetupRaw]

  val RoundTypeList =
    ListConfig[DebateRoundType](DebateRoundType.SequentialSpeechesRound(500))
  val RoundTypeConfig = SumConfig[DebateRoundType]()
  val ScoringFunctionConfig = SumConfig[ScoringFunction]()

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

  val debatePanel = new DebatePanel(S, V)

  /** Shows the user's ID. */
  def userInfoRow(roomName: String, name: String, idOpt: Option[ParticipantId]) = {
    <.div(S.userInfoRow)(
      <.span(S.userInfoMessage)(
        "You are in room ",
        <.strong(roomName),
        ", your name is ",
        <.em(name),
        idOpt.map(_.role) match {
          case None => <.span(", and you have no role!")
          case Some(role) =>
            <.span(
              ", and your role is ",
              <.strong(role.toString)
            ) // TODO stylize role
        }
      )
    )
  }

  /** Top row showing non-debating roles (user can click one to change roles).
    */
  def roleChoiceRow(
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

      <.div(S.optionBox, S.simpleSelectable, S.simpleSelected.when(isCurrent))(
        <.div(S.optionTitle)("Facilitators"),
        commaSeparatedSpans(facilitators.toList.sorted).toVdomArray,
        ^.onClick --> assumeRole(Facilitator)
      )
    }
    def observersDiv = {
      val observers = debate.participants.collect {
        case ParticipantId(name, Observer) => name
      }
      val isCurrent = observers.contains(userName)
      <.div(S.optionBox, S.simpleSelectable, S.simpleSelected.when(isCurrent))(
        <.div(S.optionTitle)("Observers"),
        commaSeparatedSpans(observers.toList.sorted).toVdomArray,
        ^.onClick --> assumeRole(Observer)
      )
    }
    // def downloadButton = <.a(
    //   ^.href := s"/download/$roomName",
    //   ^.target := "_blank",
    //   <.button(S.disconnectButton)("Download")
    // )
    def disconnectButton = <.button(S.disconnectButton)(
      "Disconnect",
      ^.onClick --> disconnect
    )

    <.div(S.roomRolesRow)(
      <.div(S.roomRolesRow)(
        facilitatorsDiv,
        observersDiv,
        // downloadButton,
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
            setupRaw.value.correctAnswerIndex,
            setupRaw.value.roles,
            System.currentTimeMillis()
          )
          sendState(
            DebateState.debate
              .set(Some(Debate(setup, Vector())))(debate)
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
                V.LiveTextField.String.mod(input = S.fullWidthInput)(answer),
                <.input(S.correctAnswerRadio)(
                  ^.`type` := "radio",
                  ^.name := "correctAnswerIndex",
                  ^.value := index,
                  ^.checked := setupRaw.value.correctAnswerIndex == index,
                  ^.onChange --> setupRaw
                    .zoomStateL(DebateSetupRaw.correctAnswerIndex)
                    .setState(index)
                ),
                <.span(S.inputRowItem)(
                  "Correct answer",
                  S.hidden.when(setupRaw.value.correctAnswerIndex != index)
                )
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

  class Backend(@unused scope: BackendScope[Unit, Unit]) {

    /** Main render method. */
    def render(@unused props: Unit, @unused state: Unit) = {
      <.div(S.app)(
        LocalConnectionSpecOpt.make(None) { connectionSpecOpt =>
          connectionSpecOpt.value match {
            case None =>
              <.div(S.lobbyContainer) (
                LocalLobby.make(Lobby.init) { lobby =>
                  MainWebSocket.make(
                    mainWebsocketUri,
                    onOpen = _ => Callback(println("Main socket opened.")),
                    onMessage = (_, msg) => lobby.setState(msg)
                  ) {
                    case MainWebSocket.Disconnected(_, reason) =>
                      <.div(S.loading)(
                        """You've been disconnected. This is probably either because of a bug or
                           because the server is restarting. Please refresh the page.
                           Sorry about that.
                        """ + reason
                      )
                    case MainWebSocket.Connecting =>
                      <.div(S.loading)("Connecting to metadata server...")
                    case MainWebSocket.Connected(sendToMainChannel, _) =>
                      def enterRoom(isScheduled: Boolean, roomName: String, participantName: String) =
                        connectionSpecOpt.setState(
                          Some(ConnectionSpec(isScheduled, roomName, participantName))
                        )
                      // TODO change page title? maybe do this on mount for the debate room component instead
                      // >> Callback(dom.window.document.title = makePageTitle(roomName)) >>
                      // Callback(dom.window.history.replaceState("", makePageTitle(roomName), roomName))
                      // roomName.setState(roomNameLive.value)

                      val noProfileString = "(select profile)"

                      LocalString.make("") { userName =>
                        <.div(S.lobbyContainer, S.spaceyContainer)(
                          <.div(^.classSet1("form-group row"))(
                            <.label(^.classSet1("col-sm-2 col-form-label"))("Profile:"),
                            V.Select.String.modFull(^.classSet1("col-sm-10 custom-select"))(
                              choices =
                                noProfileString +: lobby.value.trackedDebaters.toList.sorted,
                              curChoice =
                                if (
                                  lobby.value.trackedDebaters
                                    .contains(userName.value)
                                ) {
                                  userName.value
                                } else noProfileString,
                              setChoice = (name: String) => {
                                val adjustedName = if(name == noProfileString) "" else name
                                userName.setState(adjustedName)
                              }
                            )
                          ),
                          V.LiveTextField.String.mod(
                            span = TagMod(^.classSet1("form-group row"), ^.display.none),
                            label = ^.classSet1("col-sm-2 col-form-label"),
                            input = ^.classSet1("col-sm-10 form-control")
                          )(userName, labelOpt = Some("Name: ")),
                          <.div(^.classSet1("form-group row"), ^.display.none) {
                            val name = userName.value
                            val isDisabled = (lobby.value.trackedDebaters + "" + "(no profile)").contains(name)
                            <.button(^.classSet1("btn-block"))(
                              "Create profile",
                              ^.disabled := isDisabled,
                              (^.onClick --> sendToMainChannel(
                                RegisterDebater(userName.value)
                              )).when(!isDisabled),
                            )
                          },
                          LocalLobbyTab.make(LobbyTab.MyCurrentDebates) { lobbyTab =>
                            import LobbyTab._
                            val allMyDebates = lobby.value.scheduledRooms
                              .filter(_.assignedParticipants.contains(userName.value))
                            val myCurrentDebates = allMyDebates.filterNot(_.status.isComplete)
                            val isScheduled = lobbyTab.value match {
                              case OpenDebates => false
                              case _ => true
                            }
                            val currentRooms = lobbyTab.value match {
                              case MyCurrentDebates => myCurrentDebates
                              case AllMyDebates => allMyDebates
                              case OpenDebates => lobby.value.openRooms
                            }
                            <.div(^.classSet1("card"), ^.textAlign.center)(
                              <.div(^.classSet1("card-header"))(
                                <.ul(^.classSet1("nav nav-fill nav-tabs card-header-tabs"))(
                                  List(MyCurrentDebates, AllMyDebates, OpenDebates).toVdomArray(tab =>
                                    <.li(^.classSet1("nav-item"))(
                                      <.a(^.classSet1("nav-link", "active" -> (tab == lobbyTab.value)))(
                                        ^.href := "#",
                                        ^.onClick --> lobbyTab.setState(tab),
                                        tab.toString,
                                      )
                                    )
                                  )
                                )
                              ),
                              LocalString.make("") { roomNameLive =>
                                val canEnter = roomNameLive.value.isEmpty || userName.value.isEmpty
                                val enter = if(canEnter) enterRoom(isScheduled, roomNameLive.value, userName.value) else Callback.empty
                                <.div(^.classSet1("card-body"))(
                                  <.div(^.classSet1("input-group"))(
                                    V.LiveTextField.String.modInput(
                                      input = TagMod(
                                        ^.classSet1("form-control"),
                                        ^.onKeyDown ==> ((e: ReactKeyboardEvent) => if(e.keyCode == dom.ext.KeyCode.Enter) enter else Callback.empty)
                                      ))(roomNameLive, placeholderOpt = Some("Room")
                                    ),
                                    <.div(^.classSet1("input-group-append"))(
                                      <.button(^.classSet1("btn"))(
                                       if(currentRooms.exists(_.name == roomNameLive.value)) "Join" else "Create",
                                       ^.`type` := "button",
                                       ^.disabled := !canEnter,
                                       ^.onClick --> enter
                                      )
                                    )
                                  ),
                                  currentRooms.toVdomArray {
                                    case RoomMetadata(roomName, assignedParticipants, currentParticipants, status) =>
                                      val participantName = userName.value
                                      val canEnterRoom =
                                        participantName.nonEmpty && !currentParticipants
                                          .contains(participantName)
                                      val statusStyle = {
                                        import RoomStatus._
                                        status match {
                                          case SettingUp  => S.settingUpStatusLabel
                                          case InProgress => S.inProgressStatusLabel
                                          case Complete   => S.completeStatusLabel
                                        }
                                      }
                                      val selectableStyle =
                                        if (canEnterRoom) S.simpleSelectable
                                        else S.simpleUnselectable
                                      <.div(S.optionBox, selectableStyle)(
                                        <.div(S.optionTitle)(
                                          roomName,
                                          " ",
                                          <.span(statusStyle)(s"($status)")
                                        ),
                                        <.div(
                                          <.strong("Assigned: "),
                                          commaSeparatedSpans(
                                            assignedParticipants.toList.sorted
                                          ).toVdomArray
                                        ).when(assignedParticipants.nonEmpty),
                                        <.div(
                                          <.strong("Present: "),
                                          commaSeparatedSpans(
                                            currentParticipants.toList.sorted
                                          ).toVdomArray
                                        ).when(currentParticipants.nonEmpty),
                                        (^.onClick --> enterRoom(
                                          isScheduled,
                                          roomName,
                                          participantName
                                        )).when(canEnterRoom)
                                      )
                                  }
                                )
                              }
                            )
                          }
                        )
                      }
                  }
                }
              )
            case Some(ConnectionSpec(isScheduled, roomName, userName)) =>
              DebateRoomLocal.make(DebateState.init) { debateState =>
                DebateWebSocket.make(
                  getDebateWebsocketUri(isScheduled, roomName, userName),
                  onOpen = _ => Callback(println("Chat socket opened.")),
                  onMessage = (_, msg) => {
                    debateState.setState(msg)
                  }
                ) {
                  case DebateWebSocket.Disconnected(_, reason) =>
                    <.div(S.loading)(
                      """You've been disconnected. This is either due to a bug or the server
                         restarting. Please refresh the page. Sorry about that.
                      """ + reason
                    )
                  case DebateWebSocket.Connecting =>
                    <.div(S.loading)("Connecting to debate data server...")
                  case DebateWebSocket.Connected(sendState, _) =>
                    val userId =
                      debateState.value.participants.find(_.name == userName)
                    val backgroundStyle = S.observerBg

                    <.div(S.debateContainer)(
                      roleChoiceRow(
                        userName,
                        debateState.value,
                        sendState,
                        disconnect = connectionSpecOpt.setState(None)
                      ),
                      userInfoRow(roomName, userName, userId),
                      debateState.value.debate match {
                        case None =>
                          userId.map(_.role) match {
                            case Some(Facilitator) =>
                              facilitatorSetup(debateState.value, sendState)
                            case _ =>
                              <.div(S.debateColumn)(
                                "Waiting for a facilitator to set up the debate."
                              )
                          }
                        case Some(debate) =>
                          val setup = debate.setup
                          def assumeRole(role: Role): Callback = {
                            sendState(
                              debateState.value.addParticipant(
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
                                debateState.value.participants.collect {
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
                                      debateState.value.participants.collect {
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
                              roomName,
                              userId,
                              setup,
                              debate,
                              (d: Debate) =>
                                sendState(
                                  DebateState.debate.set(Some(d))(
                                    debateState.value
                                  )
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
