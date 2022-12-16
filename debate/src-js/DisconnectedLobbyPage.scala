package debate

import scala.language.existentials // see https://github.com/suzaku-io/diode/issues/50

import org.scalajs.dom

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import scalacss.ScalaCssReact._

import cats.~>
import cats.implicits._
import scala.concurrent.Future

object DisconnectedLobbyPage {
  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  import Helpers.ClassSetInterpolator
  val S = Styles
  val V = new jjm.ui.View(S)

  // TODO copy-pasta'd
  type DelayedFuture[A] = () => Future[A]
  val toAsyncCallback = {
    λ[DelayedFuture ~> AsyncCallback](f => AsyncCallback.fromFuture(f()))
  }

  /** performs a GET request to /leaderboard and returns a parsed Leaderboard
    * object
    */
  def loadLeaderboard(): Future[Leaderboard] = {
    org.scalajs.dom.ext.Ajax
      .get(url = "/leaderboard")
      .map(resp => io.circe.parser.decode[Leaderboard](resp.responseText))
      .flatMap {
        case Right(res) => Future.successful(res)
        case Left(fail) => Future.failed(new RuntimeException(fail))
      }
  }

  def make(
      lobby: StateSnapshot[Lobby],
      connectionSpecOpt: StateSnapshot[Option[ConnectionSpec]],
      sendToMainChannel: debate.MainChannelRequest => japgolly.scalajs.react.CallbackTo[
        Unit
      ]
  ) = {
    def enterRoom(
        isOfficial: Boolean,
        roomName: String,
        participantName: String
    ) =
      connectionSpecOpt.setState(
        Some(
          ConnectionSpec(isOfficial, roomName, participantName)
        )
      )
    // TODO change page title? maybe do this on mount for the debate room component instead
    // >> Callback(dom.window.document.title = makePageTitle(roomName)) >>
    // Callback(dom.window.history.replaceState("", makePageTitle(roomName), roomName))
    // roomName.setState(roomNameLive.value)

    val noProfileString = "(select profile)"
    val profileCookieId = "debate-profile"

    def setChoice(
        userName: StateSnapshot[String]
    )(name: String) = {
      val adjustedName =
        if (name == noProfileString) "" else name
      userName.setState(adjustedName) >> Callback(
        setCookie(
          profileCookieId,
          adjustedName,
          expires = 5
        )
      )
    }

    def curChoice(userName: StateSnapshot[String]) = {
      if (
        lobby.value.trackedDebaters
          .contains(userName.value)
      ) {
        userName.value
      } else noProfileString
    }

    def createProfileButton(userName: StateSnapshot[String]) = {
      <.div(c"form-group row", Styles.adminOnly) {
        val name = userName.value
        val isDisabled =
          (lobby.value.trackedDebaters + "" + "(no profile)")
            .contains(name)
        <.button(c"btn btn-primary btn-block")(
          "Create profile",
          ^.disabled := isDisabled,
          (^.onClick --> sendToMainChannel(
            RegisterDebater(userName.value)
          )).when(!isDisabled)
        )
      }
    }

    def deleteProfileButton(
        userName: StateSnapshot[String]
    ) = {
      <.div(c"form-group row", Styles.adminOnly) {
        val name = userName.value
        val isEnabled =
          lobby.value.trackedDebaters.contains(name)
        <.button(c"btn btn-danger btn-block")(
          "Delete profile",
          ^.disabled := !isEnabled,
          (^.onClick --> sendToMainChannel(
            RemoveDebater(userName.value)
          )).when(isEnabled)
        )
      }
    }

    def joinOrCreateDebate(
        currentRooms: Vector[RoomMetadata],
        isOfficial: Boolean,
        canEnter: Boolean,
        enter: Callback,
        roomNameLive: StateSnapshot[String]
    ) = {
      <.div(c"input-group-append")(
        <.button(c"btn btn-primary")(
          (
            (if (
               currentRooms.exists(
                 _.name == roomNameLive.value
               )
             ) "Join"
             else "Create") +
              " " +
              (if (isOfficial) "Official Debate"
               else "Practice Debate")
          ),
          ^.`type` := "button",
          ^.disabled := !canEnter,
          ^.onClick --> enter
        )
      )
    }

    def roomManagement(
        roomMetadata: RoomMetadata,
        isOfficial: Boolean,
        userName: StateSnapshot[String]
    ) = {
      val canEnterRoom =
        userName.value.nonEmpty && !roomMetadata.currentParticipants
          .contains(userName.value)
      val statusStyle = {
        import RoomStatus._
        roomMetadata.status match {
          case SettingUp => S.settingUpStatusLabel
          case InProgress =>
            S.inProgressStatusLabel
          case Complete => S.completeStatusLabel
        }
      }

      // TODO probably just a val instead of a def
      def statusDisplay(status: debate.RoomStatus) = {
        <.div(S.optionTitle)(
          roomMetadata.name,
          " ",
          <.span(statusStyle)(s"($status)")
        )
      }

      def assignedParticipants() = {
        <.div(
          <.strong("Assigned: "),
          Helpers
            .commaSeparatedSpans(
              roomMetadata.assignedParticipants.toList.sorted
            )
            .toVdomArray
        ).when(roomMetadata.assignedParticipants.nonEmpty)
      }

      def presentParticipants() = {
        <.div(
          <.strong("Present: "),
          Helpers
            .commaSeparatedSpans(
              roomMetadata.currentParticipants.toList.sorted
            )
            .toVdomArray
        ).when(roomMetadata.currentParticipants.nonEmpty)
      }

      def deleteRoom() = {
        <.button(
          c"btn btn-block btn-danger",
          S.adminOnly
        )(
          "Delete room",
          ^.onClick ==> ((e: ReactMouseEvent) => {
            e.stopPropagation();
            sendToMainChannel(
              DeleteRoom(isOfficial, roomMetadata.name)
            )
          })
        )
      }

      def enterRoomButton() = {
        (^.onClick --> enterRoom(
          isOfficial,
          roomMetadata.name,
          userName.value
        )).when(canEnterRoom)
      }

      val selectableStyle =
        if (canEnterRoom) S.simpleSelectable
        else S.simpleUnselectable
      val status = roomMetadata.status
      <.div(S.optionBox, selectableStyle)(
        statusDisplay(status = status),
        assignedParticipants(),
        presentParticipants(),
        deleteRoom(),
        enterRoomButton()
      )
    }

    import jjm.ui.LocalState

    sealed trait LobbyTab extends Product with Serializable {
      import LobbyTab._
      override def toString = this match {
        case MyDebates          => "My Debates"
        case AllOfficialDebates => "All Official Debates"
        case PracticeDebates    => "Practice Debates"
        case Leaderboard        => "Leadeboard"
      }
    }
    object LobbyTab {
      case object MyDebates extends LobbyTab
      case object AllOfficialDebates extends LobbyTab
      case object PracticeDebates extends LobbyTab
      case object Leaderboard extends LobbyTab
    }

    val LocalString = new LocalState[String]

    val LocalLobbyTab = new LocalState[LobbyTab]

    def profileSelector(userName: StateSnapshot[String]) = {
      <.div(c"form-group row")(
        <.label(c"col-sm-2 col-form-label")("Profile:"),
        V.Select.String.modFull(
          TagMod(c"col-sm-10", S.customSelect)
        )(
          choices =
            noProfileString +: lobby.value.trackedDebaters.toList.sorted,
          curChoice = curChoice(userName),
          setChoice = setChoice(userName)
        )
      )
    }

    def nameInput(userName: StateSnapshot[String]) = {
      V.LiveTextField.String.mod(
        span = TagMod(c"form-group row", Styles.adminOnly),
        label = c"col-sm-2 col-form-label",
        input = c"col-sm-10 form-control"
      )(userName, labelOpt = Some("Name: "))
    }

    LocalString.make(initialValue = getCookie(profileCookieId).getOrElse("")) {
      userName =>
        <.div(S.lobbyContainer, S.spaceyContainer)(
          profileSelector(userName = userName),
          nameInput(userName = userName),
          createProfileButton(userName),
          deleteProfileButton(userName),
          LocalLobbyTab.make(LobbyTab.MyDebates) { lobbyTab =>
            import LobbyTab._
            val myDebates = lobby.value.officialRooms
              .filter(
                _.assignedParticipants.contains(userName.value)
              )
            val isOfficial = lobbyTab.value match {
              case PracticeDebates => false
              case _               => true
            }
            val currentRooms = lobbyTab.value match {
              case MyDebates          => myDebates
              case AllOfficialDebates => lobby.value.officialRooms
              case PracticeDebates    => lobby.value.practiceRooms
              case Leaderboard        => Vector.empty
            }

            def lobbySelector() = {
              List(
                MyDebates,
                AllOfficialDebates,
                PracticeDebates,
                Leaderboard
              ).toVdomArray(tab =>
                <.li(c"nav-item")(
                  <.a(
                    ^.classSet1(
                      "nav-link",
                      "active" -> (tab == lobbyTab.value)
                    )
                  )(
                    ^.href := "#",
                    ^.onClick --> lobbyTab.setState(tab),
                    tab.toString
                  )
                )
              )
            }

            def roomNameInput(
                roomNameLive: StateSnapshot[String],
                enter: Callback
            ) = {
              V.LiveTextField.String.modInput(
                input = TagMod(
                  c"form-control",
                  ^.onKeyDown ==> ((e: ReactKeyboardEvent) =>
                    if (e.keyCode == dom.ext.KeyCode.Enter) enter
                    else Callback.empty
                  )
                )
              )(
                roomNameLive,
                placeholderOpt = Some("Room")
              )
            }

            // TODO clean up this lol
            case class RowEntry(name: String, wins: Int, losses: Int)
            case class State(
                judge: List[RowEntry],
                honest: List[RowEntry],
                dishonest: List[RowEntry]
            )

            def sortTableBy[T, B](
                ref: StateSnapshot[State],
                getter: State => List[T],
                maker: (State, List[T]) => State,
                by: T => B
            )(implicit ordering: Ordering[B]) = {
              <.div()(
                <.i(c"bi bi-caret-down-fill")(
                  ^.onClick --> {
                    val newList = getter(ref.value).sortBy(by)
                    val newValue = maker(ref.value, newList)
                    ref.setState(newValue)
                  }
                ),
                <.i(c"bi bi-caret-up-fill")(
                  ^.onClick --> {
                    implicit val reverseOrdering: Ordering[B] =
                      ordering.reverse
                    val newList = getter(ref.value).sortBy(by)(reverseOrdering)
                    val newValue = maker(ref.value, newList)
                    ref.setState(newValue)
                  }
                )
              )
            }

            def leaderboardTable() = {

              val Cls = new LeaderboardTable[State]

              def renderRowEntries(
                  ref: StateSnapshot[
                    State
                  ], // used to make callbacks for sorting,
                  getter: State => List[RowEntry],
                  maker: (State, List[RowEntry]) => State
              ) = {
                def ourSort[B](
                    by: RowEntry => B
                )(implicit ordering: Ordering[B]) =
                  sortTableBy(ref, getter, maker, by)

                <.table(c"table table-striped")(
                  <.thead(
                    <.tr(
                      <.th(
                        "Name",
                        ourSort(_.name)
                      ),
                      <.th(
                        "Wins",
                        ourSort(_.wins)
                      ),
                      <.th(
                        "Losses",
                        ourSort(_.losses)
                      ),
                      // TODO i think sorting is broken?
                      <.th(
                        "Win %",
                        ourSort(x => { x.wins.toDouble / (x.losses + x.wins) })
                      )
                    )
                  ),
                  <.tbody(
                    getter(ref.value)
                      // .sortBy(-_._2.wins.toDouble / _._2.losses)
                      .toVdomArray { case rowEntry =>
                        <.tr(
                          <.td(rowEntry.name),
                          <.td(rowEntry.wins),
                          <.td(rowEntry.losses),
                          <.td(
                            f"${rowEntry.wins.toDouble / (rowEntry.losses + rowEntry.wins) * 100}%.1f"
                          )
                        )
                      }
                  )
                )
              }

              def render(s: StateSnapshot[State]) = {
                <.div(
                  <.h3("Judge"),
                  renderRowEntries(
                    ref = s,
                    getter = _.judge,
                    maker = (x, y) => x.copy(judge = y)
                  ),
                  <.h3("Honest"),
                  renderRowEntries(
                    ref = s,
                    getter = _.honest,
                    maker = (x, y) => x.copy(honest = y)
                  ),
                  <.h3("Dishonest"),
                  renderRowEntries(
                    ref = s,
                    getter = _.dishonest,
                    maker = (x, y) => x.copy(dishonest = y)
                  )
                )
              }

              def leaderboardToRows(
                  x: LeaderboardForRoleType
              ) = {
                val init: List[RowEntry] = List()
                x.perProfile.foldLeft(init) {
                  case ((acc: List[RowEntry]), (k: String, v: List[Double])) =>
                    val wins = v.filter(_ > 0.5).length
                    val losses = v.filter(_ < 0.5).length
                    acc :+ RowEntry(k, wins, losses)
                }
              }

              // TODO is this right? i think im confused about callbacks
              def onMount: AsyncCallback[State] = {
                // TODO mutate
                (for {
                  f <- AsyncCallback.fromFuture(loadLeaderboard())
                  _ = println(f)
                  _ = println("i love debugging, PSYCH!")
                } yield State(
                  judge = leaderboardToRows(f.judge),
                  honest = leaderboardToRows(f.honest),
                  dishonest = leaderboardToRows(f.dishonest)
                ))
              }

              Cls.make(
                initialValue =
                  State(judge = List(), honest = List(), dishonest = List()),
                render = render,
                onMount = onMount,
                shouldRefresh = _ => true
              )
            }

            <.div(c"card", ^.textAlign.center)(
              <.div(c"card-header")(
                <.ul(c"nav nav-fill nav-tabs card-header-tabs")(lobbySelector())
              ),
              LocalString.make("") { roomNameLive =>
                val canEnter =
                  roomNameLive.value.nonEmpty && userName.value.nonEmpty
                val enter =
                  if (canEnter)
                    enterRoom(
                      isOfficial,
                      roomNameLive.value,
                      userName.value
                    )
                  else Callback.empty
                <.div(c"card-body", S.spaceySubcontainer)(
                  <.div(c"input-group", ^.width.auto)(
                    roomNameInput(enter = enter, roomNameLive = roomNameLive),
                    joinOrCreateDebate(
                      currentRooms = currentRooms,
                      isOfficial = isOfficial,
                      canEnter = canEnter,
                      enter = enter,
                      roomNameLive = roomNameLive
                    )
                  ).when(
                    lobbyTab.value != MyDebates && lobbyTab.value != Leaderboard
                  ),
                  <.div("No rooms to show.")
                    .when(
                      currentRooms.isEmpty && lobbyTab.value != Leaderboard
                    ),
                  leaderboardTable()
                    .when(lobbyTab.value == Leaderboard),
                  currentRooms.toVdomArray { case rm: RoomMetadata =>
                    roomManagement(
                      roomMetadata = rm,
                      isOfficial = isOfficial,
                      userName = userName
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
