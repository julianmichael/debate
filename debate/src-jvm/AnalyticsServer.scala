package debate

import cats.effect._
import cats.implicits._

import _root_.io.circe.Json
import org.http4s.client.Client

import debate.service.AnalyticsService

object AnalyticsServer {
  // case class Handle(port: Int, restart: IO[Unit], shutdown: IO[Unit])
  // def startPythonServer(port: Int)(implicit cs: ContextShift[IO]): Resource[IO, Handle] =
  //   Resource.make[IO, Handle](
  //     IO {
  //       val x = IO.unit.start
  //       import sys.process._
  //       val p = Process("ls")
  //       // p.
  //       Handle(
  //         port = port,
  //         restart = IO.unit, // TODO
  //         shutdown = IO.unit // TODO
  //       )
  //     }
  //   )(_.shutdown)

  // def makeClientOfPythonServer(handle: Handle, httpClient: Client[IO]): AnalyticsService[IO] = {
  def makeClientOfPythonServer(
    port: Int,
    httpClient: Client[IO],
    stateManager: DebateStateManager,
    blocker: Blocker
  )(implicit cs: ContextShift[IO]): AnalyticsService[IO] = {
    import org.http4s.{Request => HttpRequest}
    import org.http4s._

    val baseUri = Uri(
      scheme = Some(Uri.Scheme.http),
      authority = Some(Uri.Authority(port = Some(port)))
    )

    import org.http4s.circe._

    def doRequest(req: AnalyticsService.Request, httpRequest: HttpRequest[IO]): IO[req.Out] =
      httpClient
        .expect[Json](httpRequest)
        .flatMap { responseJson =>
          IO.fromEither(
            AnalyticsService.Request.analyticsServiceRequestDotDecoder(req)(responseJson.hcursor)
          )
        }

    def get(uri: Uri)  = HttpRequest[IO](method = Method.GET, uri = uri)
    def post(uri: Uri) = HttpRequest[IO](method = Method.POST, uri = uri)

    new AnalyticsService[IO] {
      def refresh =
        stateManager.writeCSVs(blocker) >>
          doRequest(AnalyticsService.Request.Refresh, post(baseUri.withPath("/refresh")))
      def getAnalyticsGraphNames = doRequest(
        AnalyticsService.Request.GetAnalyticsGraphNames,
        get(baseUri.withPath("/all_graphs"))
      )
      def getAnalyticsGraph(name: String) = doRequest(
        AnalyticsService.Request.GetAnalyticsGraph(name),
        get(baseUri.withPath(s"/graph/$name"))
      )
    }
  }

}
