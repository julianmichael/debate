package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object Serve
    extends CommandIOApp(name = "mill -i debate.jvm.run", header = "Serve the live chat app.") {

  val jsPathO = Opts
    .option[NIOPath]("js", metavar = "path", help = "Where to get the JS main file.")

  val jsDepsPathO = Opts
    .option[NIOPath]("jsDeps", metavar = "path", help = "Where to get the JS dependencies file.")

  val portO = Opts
    .option[Int]("port", metavar = "port number", help = "Port where to host the server.")
    .withDefault(8080)

  val analyticsPortO = Opts
    .option[Int](
      "analytics-port",
      metavar = "port number",
      help = "Port where to host the analytics server."
    )
    .withDefault(8081)

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "directory path",
      help = "Directory in which to save the debates."
    )
    .withDefault(Paths.get("save"))

  val sslO = Opts.flag("ssl", help = "Whether to use SSL encryption/host over HTTPS.").orFalse

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = (jsPathO, jsDepsPathO, portO, analyticsPortO, saveO, sslO).mapN {
    (jsPath, jsDepsPath, port, analyticsPort, save, ssl) =>
      Blocker[IO]
        .use { blocker =>
          Server
            .create(Paths.get("data"), save, blocker)
            .flatMap(_.run(jsPath, jsDepsPath, port, analyticsPort, executionContext, ssl))
        }
        .as(ExitCode.Success)
  }

}
