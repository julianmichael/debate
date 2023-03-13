package debate
package view.lobby

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import cats.implicits._
import cats.~>

import japgolly.scalajs.react.AsyncCallback
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.jquery.jQuery
import scalacss.ScalaCssReact._

import jjm.OrWrapped
import jjm.io.HttpUtil
import jjm.ui.CacheCallContent

import debate.util.ListConfig
import debate.util.Local
import debate.service.AnalyticsService

object AnalyticsPanel {
  val S = Styles
  val V = new jjm.ui.View(S)

  import Utils.ClassSetInterpolator

  import org.scalajs.macrotaskexecutor.MacrotaskExecutor.Implicits._

  val analyticsService = {

    def httpProtocol = dom.document.location.protocol
    type DelayedFuture[A] = () => Future[A]
    val toAsyncCallback = λ[DelayedFuture ~> AsyncCallback](f => AsyncCallback.fromFuture(f()))

    val analyticsApiUrl: String =
      s"$httpProtocol//${dom.document.location.host}/$analyticsServiceApiEndpoint"

    AnalyticsService(
      HttpUtil
        .makeHttpPostClient[AnalyticsService.Request](analyticsApiUrl)
        .andThenK(toAsyncCallback)
        // .andThenK(λ[AsyncCallback ~> OrWrapped[AsyncCallback, *]](OrWrapped.wrapped(_)))
    )
  }

  val GraphNamesFetch = new CacheCallContent[Unit, Vector[String]]
  val GraphNameSelect = V.OptionalSelect[String](identity, "Choose...")

  def makeGraphNamePretty(endpoint: String) = endpoint.replaceAll("_", " ")
  def makeEndpointName(graphName: String)   = graphName.replaceAll(" ", "_")

  def getGraphDisplayDivId(index: Int) = s"graph-$index"

  def reloadGraphs(oldGraphs: Vector[Option[String]], newGraphs: Vector[Option[String]]) =
    newGraphs
      .zipWithIndex
      .collect { case (newGraphNameOpt, index) =>
        oldGraphs.lift(index) match {
          case Some(`newGraphNameOpt`) =>
            Callback.empty // don't need to refresh this graph
          case _ =>
            val graphId = getGraphDisplayDivId(index)
            def jqDiv   = jQuery(s"#$graphId")
            newGraphNameOpt match {
              case None =>
                Callback(jqDiv.empty()) // empty the graph display
              case Some(graphName) =>
                analyticsService
                  .getAnalyticsGraph(makeEndpointName(graphName))
                  .completeWith {
                    case Failure(exception) =>
                      Callback(jqDiv.html(exception.getMessage()))
                    case Success(data) =>
                      Callback(
                        scalajs
                          .js
                          .Dynamic
                          .global
                          .vegaEmbed(s"#$graphId", io.circe.scalajs.convertJsonToJs(data))
                      )
                  }
            }
        }
      }
      .combineAll

  def apply() =
    Local[Vector[Option[String]]].make(Vector(None), didUpdate = reloadGraphs) { graphs =>
      <.div(c"card-body", S.spaceySubcontainer)(
        <.h3(c"card-title")("Analytics"),
        <.div(c"pb-2")(
          <.a(c"card-link")(
            ^.href := "#",
            "Refresh",
            ^.onClick -->
              (analyticsService.refresh.completeWith(_ => reloadGraphs(Vector(), graphs.value)))
          ),
          <.a(c"card-link")(^.href := "/download", ^.target.blank, "Download")
        ),
        GraphNamesFetch.make((), _ => OrWrapped.wrapped(analyticsService.getAnalyticsGraphNames)) {
          case GraphNamesFetch.Loading =>
            <.div("Loading graph names.")
          case GraphNamesFetch.Loaded(graphNames) =>
            ListConfig[Option[String]].nice(graphs, None, 1) {
              case ListConfig.Context(graphName, index) =>
                ReactFragment(
                  GraphNameSelect.mod(select = TagMod(S.listCardHeaderSelect))(
                    graphNames.toSet.map(makeGraphNamePretty),
                    graphName
                  ),
                  <.div(c"card-body")(^.id := getGraphDisplayDivId(index))
                )
            }
        }
      )
    }
}
