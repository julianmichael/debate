package debate

import java.nio.file.{Path => NIOPath}

import cats.effect.ExitCode
import cats.effect.IO
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

import jjm.io.FileUtil

object Convert
    extends CommandIOApp(name = "mill -i debate.jvm.run", header = "Serve the live chat app.") {

  val inO = Opts.option[NIOPath]("in", metavar = "path", help = "where to read in the old debates.")

  val outO = Opts
    .option[NIOPath]("out", metavar = "path", help = "where to output the new debates.")

  def main: Opts[IO[ExitCode]] = (inO, outO).mapN { (in, out) =>
    val inDirOs = os.Path(in, os.pwd)
    for {
      files <- IO(os.list(inDirOs).map(_.toNIO).filter(_.toString.endsWith(".json")).toVector)
      _ <- files.traverse { path =>
        for {
          debate <- FileUtil.readJson[Debate](path)
          debate2 = Debate2.fromDebate(debate)
          _ <- FileUtil.writeJson(out.resolve(path.getFileName))(debate2)
        } yield ()
      }
    } yield ExitCode.Success
  }
}
