package debate
package view.lobby

import japgolly.scalajs.react.AsyncCallback
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.{all => Optics}
import org.scalajs.dom
import scalacss.ScalaCssReact._

import jjm.ui.Mounting

import debate.util.ListConfig
import debate.util.SyncedState
import japgolly.scalajs.react.feature.ReactFragment

object OpenEndedFeedbackPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  val SyncedFeedback = SyncedState
    .forJsonString[OpenEndedFeedback, OpenEndedFeedback, OpenEndedFeedback](
      getRequestFromState = identity,
      getStateUpdateFromResponse = responseState => _ => responseState
    )

  val openEndedFeedbackUri: String =
    s"${Utils.wsProtocol}//${dom.document.location.host}/feedback-ws"

  import Utils.ClassSetInterpolator

//   import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  def apply(userName: String) =
    SyncedFeedback.make(websocketURI = openEndedFeedbackUri) {
      case SyncedFeedback.Disconnected(reconnect, reason) =>
        Mounting.make(AsyncCallback.unit.delayMs(5000).completeWith(_ => reconnect))(
          <.div(S.loading)("""You've been disconnected. Will attempt to reconnect every 5 seconds.
                    If you don't reconnect after a few seconds,
                    Please refresh the page. """ + reason)
        )
      case SyncedFeedback.Connecting =>
        <.div(S.loading)("Connecting to feedback data service...")
      case SyncedFeedback.Connected(_, None) =>
        <.div(S.loading)("Waiting for feedback data...")
      case SyncedFeedback.Connected(_, Some(feedback)) =>
        ListConfig[OpenEndedFeedbackSection].nice(
          feedback.zoomStateL(OpenEndedFeedback.sections),
          OpenEndedFeedbackSection("", Vector()),
          0
        ) { case ListConfig.Context(item, _) =>
          ReactFragment(
            <.div(c"card-header")(
              V.LiveTextField.String(item.zoomStateL(OpenEndedFeedbackSection.name))
            ),
            <.div(c"card-body")(
              ListConfig[OpenEndedFeedbackQuestion].nice(
                item.zoomStateL(OpenEndedFeedbackSection.questions),
                OpenEndedFeedbackQuestion("", Map()),
                0
              ) { case ListConfig.Context(question, _) =>
                def makeVoteDiv(vote: Boolean) =
                  <.div(c"mt-1")(
                    <.button(c"mx-1")(
                      c"btn",
                      if (question.value.responses.get(userName).exists(_ == vote))
                        if (vote)
                          c"btn-success"
                        else
                          c"btn-danger"
                      else if (vote)
                        c"btn-outline-success"
                      else
                        c"btn-outline-danger"
                    )(
                      if (vote)
                        <.i(c"bi bi-hand-thumbs-up")
                      else
                        <.i(c"bi bi-hand-thumbs-down"),
                      ^.onClick -->
                        question.modState(
                          OpenEndedFeedbackQuestion
                            .responses
                            .composeLens(Optics.at(userName))
                            .modify {
                              case Some(`vote`) =>
                                None
                              case _ =>
                                Some(vote)
                            }
                        )
                    ),
                    Utils
                      .delimitedSpans(
                        question.value.responses.filter(_._2 == vote).keySet.toVector.sorted
                      )
                      .toVdomArray
                  )

                <.div(c"pb-1")(
                  V.LiveTextField.String(question.zoomStateL(OpenEndedFeedbackQuestion.question)),
                  makeVoteDiv(true),
                  makeVoteDiv(false)
                )
              }
            )
          )
        }

    }

}
