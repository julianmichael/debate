package debate

import java.io.InputStream
import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths
import java.security.KeyStore
import java.security.SecureRandom

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.TrustManagerFactory
import org.http4s.server.blaze.BlazeServerBuilder

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
    .option[Int]("port", metavar = "<port number>", help = "Port where to host the server.")
    .withDefault(8080)

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "<directory path>",
      help = "Directory in which to save the debates."
    )
    .withDefault(Paths.get("save"))

  val sslO = Opts.flag("ssl", help = "Whether to use SSL encryption/host over HTTPS.").orFalse

  def getBuilder(ssl: Boolean) =
    if (!ssl)
      IO.pure(BlazeServerBuilder[IO](executionContext))
    else {
      getSslContext.attempt >>= {
        case Right(sslContext) =>
          IO.pure(BlazeServerBuilder[IO](executionContext).withSslContext(sslContext))
        case Left(e) =>
          IO(System.err.println(s"HTTPS Configuration failed: ${e.getMessage}"))
            .as(BlazeServerBuilder[IO](executionContext))
      }
    }

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = (jsPathO, jsDepsPathO, portO, saveO, sslO).mapN {
    (jsPath, jsDepsPath, port, save, ssl) =>
      for {
        builder <- getBuilder(ssl)
        _ <- Blocker[IO].use { blocker =>
          Server
            .create(Paths.get("data"), save, blocker)
            .map(_.httpApp(jsPath, jsDepsPath))
            .flatMap(app => builder.bindHttp(port, "0.0.0.0").withHttpApp(app).serve.compile.drain)
        }
      } yield ExitCode.Success
  }

  /** Get the info necessary to host using HTTPS. Looks for `keystore.jks` and
    * `password` files in the JVM resources. (Probably better to redirect from
    * an HTTPS reverse proxy instead)
    */
  val getSslContext =
    for {
      password <- IO(
        new java.util.Scanner(getClass.getClassLoader.getResourceAsStream("password"))
          .next
          .toCharArray
      )
      keystore <- IO {
        val keystoreInputStream: InputStream = getClass
          .getClassLoader
          .getResourceAsStream("keystore.jks")
        require(keystoreInputStream != null, "Keystore required!")
        val keystore: KeyStore = KeyStore.getInstance("jks")
        keystore.load(keystoreInputStream, password)
        keystore
      }
      sslContext <- IO {
        val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
        keyManagerFactory.init(keystore, password)

        val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
        tmf.init(keystore)

        val context = SSLContext.getInstance("TLS")
        context.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
        context
      }
    } yield sslContext
}
