package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

import jjm.io.FileUtil
import jjm.implicits._

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object WriteDebates
    extends CommandIOApp(
      name = "mill -i debate.jvm.runMain debate.WriteDebates",
      header = "Write the debates into a more human/LM-readable format."
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

  val metadataOutO = Opts
    .option[NIOPath](
      "metadata-out",
      metavar = "file path",
      help = "file in which to save the debate metadata."
    )
    .withDefault(Paths.get("debates-metadata.json"))

  val roomNameOptO =
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
  def main: Opts[IO[ExitCode]] = (saveO, outO, metadataOutO, roomNameOptO).mapN {
    (save, out, metadataOut, roomNameOpt) =>
      Blocker[IO].use { blocker =>
        for {
          server <- Server.create(Paths.get("data"), save, Nil, blocker)
          rooms  <- server.officialDebates.rooms.get
          _ <- {
            val readableDebates =
              rooms
                .view
                .filter(p => roomNameOpt.forall(_ == p._1))
                .flatMap { case (roomName, room) =>
                  ReadableDebate.sessionsFromDebate(
                    roomName,
                    room.debate.debate,
                    quoteDelimiters = ("<quote>", "</quote>"),
                    renderQuoteIndices =
                      span => s"<indices> (${span.begin}-${span.endExclusive})</indices>",
                    liveOnly = true
                  )
                }
                .toList
            FileUtil.writeJsonLines(out)(readableDebates)
          }
          metadata <- server.officialDebates.getRoomMetadata
          filteredDebates = {
            val unbalancedDebates = AnalyzeResults.getDebatesFilteredForTime(
              rooms.mapVals(_.debate.debate).toList,
              server.qualityDataset
            )
            AnalyzeResults.balanceDebates(unbalancedDebates)
          }
          browserMetadata = rooms.map { case (roomName, room) =>
            BrowserRoomMetadata
              .fromDebate(room.debate.debate, room.debate.metadata(roomName), filteredDebates)
          }
          _ <- FileUtil.writeJson(metadataOut)(browserMetadata.toList)
        } yield ExitCode.Success
      }
  }

}

import monocle.macros.Lenses
import io.circe.generic.JsonCodec

@Lenses
@JsonCodec
case class BrowserRoomMetadata(
  setting: DebateSetting,
  name: String,
  sourceMaterialId: SourceMaterialId,
  storyTitle: String,
  question: String,
  roleAssignments: Map[LiveDebateRole, String],
  ultimateRoles: Map[LeaderboardCategory, String],
  offlineJudges: Map[String, OfflineJudgingMode],
  creationTime: Long,
  status: RoomStatus,
  latestUpdateTime: Long,
  currentSpeakers: Set[LiveDebateRole],
  includedInPaper: Boolean
)
object BrowserRoomMetadata {
  def fromDebate(
    debate: Debate,
    metadata: RoomMetadata,
    includedDebates: List[AnalyzeResults.AnalyzedOnlineDebate]
  ): BrowserRoomMetadata = BrowserRoomMetadata(
    DebateSetting.fromDebate(debate),
    metadata.name,
    metadata.sourceMaterialId,
    metadata.storyTitle,
    metadata.question,
    metadata.roleAssignments,
    metadata
      .roleAssignments
      .map { case (k, v) =>
        val category =
          k match {
            case Judge =>
              LeaderboardCategory.Judge
            case Debater(i) =>
              if (debate.setup.correctAnswerIndex == i)
                LeaderboardCategory.HonestDebater
              else
                LeaderboardCategory.DishonestDebater
          }
        category -> v.toString
      }, // ++
    //  debate.offlineJudgingResults.keySet.toList.map(LeaderboardCategory.OfflineJudge -> _).toMap,
    metadata
      .offlineJudgeAssignments
      .toList
      .flatMap { case (k, v) =>
        v.map(k -> _)
      }
      .toMap,
    metadata.creationTime,
    metadata.status,
    metadata.latestUpdateTime,
    metadata.currentSpeakers,
    includedDebates.exists(_.roomName == metadata.name)
  )
}
