package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

import jjm.io.FileUtil

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object WriteDebates
    extends CommandIOApp(
      name = "mill -i debate.jvm.runMain debate.WriteDebates",
      header = "Write the debates into a more LM-readable format."
    ) {

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "directory path",
      help = "Directory from which to read the debates."
    )
    .withDefault(Paths.get("save"))

  val outO = Opts
    .option[NIOPath](
      "out",
      metavar = "file path",
      help = "file in which to save the LM-readable debates."
    )
    .withDefault(Paths.get("debates-readable.jsonl"))

  val roomO =
    Opts
      .option[String](
        "room",
        metavar = "room name",
        help = "Room name to print out the debate for."
      )
      .orNone

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = (saveO, outO, roomO).mapN { (save, out, roomName) =>
    Blocker[IO].use { blocker =>
      for {
        server <- Server.create(Paths.get("data"), save, Nil, blocker)
        rooms  <- server.officialDebates.rooms.get
        _ <- {
          val readableDebates =
            rooms
              .view
              .filter(p => roomName.forall(_ == p._1))
              .flatMap { case (_, room) =>
                ReadableDebate.sessionsFromDebate(room.debate.debate, ("<quote>", "</quote>"))
              }
              .toList
          FileUtil.writeJsonLines(out)(readableDebates)
        }
      } yield ExitCode.Success
    }
  }

}
