package debate

import java.nio.file.{Path => NIOPath}
import java.nio.file.Paths

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

import jjm.io.FileUtil
import cats.Applicative

/** Main object for running the debate webserver. Uses the decline-effect
  * package for command line arg processing / app entry point.
  */
object AnonymizeDebates
    extends CommandIOApp(
      name = "mill -i debate.jvm.runMain debate.AnonymizeDebates",
      header = "Anonymize human profiles."
    ) {

  val saveO = Opts
    .option[NIOPath](
      "save",
      metavar = "directory path",
      help = "Directory from which to read the debates."
    )
    .withDefault(Paths.get("save"))

  val anonMappingOutO = Opts
    .option[NIOPath](
      "anon-mapping-out",
      metavar = "file path",
      help = "Path where to save the anonymization mapping."
    )
    .withDefault(Paths.get("save-anonymized/anon-mapping.json"))

  val outO = Opts
    .option[NIOPath](
      "out",
      metavar = "directory path",
      help = "Directory where to save the anonymized debates."
    )
    .withDefault(Paths.get("save-anonymized"))

  // courtesy of GPT-4, with some light editing
  val anonymousNames = List(
    "Archie Artichoke",
    "Bubbles Brandywine",
    "Crispy Crumbleton",
    "Danny Duckworth",
    "Elmo Eggplant",
    "Fizzlewick Fandango",
    "Gigglesworth Grapevine",
    "Hopsy Hummingbird",
    "Izzy Icetoes",
    "Jolly Jumblebee",
    "Kiki Kiwifruit",
    "Lulu Lemonade",
    "Muffin Mumblecrust",
    "Nibbles Nuggetson",
    "Osmanth Oatcake",
    "Pippy Puddlejump",
    "Quibble Quackenbush",
    "Razzle Rumplesnitz",
    "Snicker Snoodle",
    "Tazzy Tumbleweed",
    "Umpy Umbrellabird",
    "Vinnie Vanilla",
    "Wabble Wumbleton",
    "Xerxes Xylophone",
    "Yippy Yapperson",
    "Zany Zoodlepuff"
  )

  /** Main function. Runs the server. Stop with ^C.
    *
    * @return
    *   the process's exit code.
    */
  def main: Opts[IO[ExitCode]] = (saveO, outO, anonMappingOutO).mapN {
    (save, out, anonMappingOut) =>
      val officialDebatesPath = out.resolve("official")
      Blocker[IO].use { blocker =>
        for {
          server   <- Server.create(Paths.get("data"), save, Nil, blocker)
          profiles <- server.profiles.get
          aiProfiles = profiles.filter(_._2.isAI).keySet
          rooms <- server.officialDebates.rooms.get
          _     <- fs2.io.file.createDirectories[IO](blocker, officialDebatesPath)
          allParticipants = (new scala.util.Random()).shuffle(
            rooms
              .unorderedFoldMap(room => debateParticipants.getAll(room.debate.debate).toSet)
              .toList
              .filter(!aiProfiles.contains(_))
          )
          anonMapping =
            allParticipants.zip(anonymousNames).toMap ++ aiProfiles.view.map(x => x -> x).toMap
          _ <- FileUtil.writeJson(anonMappingOut)(anonMapping)
          _ <- rooms
            .toList
            .traverse { case (roomName, room) =>
              FileUtil.writeJson(officialDebatesPath.resolve(roomName + ".json"))(
                anonymizeParticipants(anonMapping, room.debate.debate)
              )
            }
        } yield ExitCode.Success
      }
  }

  import monocle.Traversal

  def mapValuesTraversal[K, V]: Traversal[Map[K, V], V] =
    new Traversal[Map[K, V], V] {
      def modifyF[F[_]: cats.Applicative](f: V => F[V])(s: Map[K, V]): F[Map[K, V]] = s
        .toList
        .traverse { case (k, v) =>
          f(v).map(k -> _)
        }
        .map(_.toMap)
    }

  def mapKeysTraversalUnsafe[K, V]: Traversal[Map[K, V], K] =
    new Traversal[Map[K, V], K] {
      def modifyF[F[_]: cats.Applicative](f: K => F[K])(s: Map[K, V]): F[Map[K, V]] = s
        .toList
        .traverse { case (k, v) =>
          f(k).map(_ -> v)
        }
        .map(_.toMap)
    }

  val debateSetupParticipants: Traversal[DebateSetup, String] =
    new Traversal[DebateSetup, String] {
      def modifyF[F[_]: cats.Applicative](
        f: String => F[String]
      )(s: DebateSetup): F[DebateSetup] = {
        val rolesA = mapValuesTraversal[LiveDebateRole, String].modifyF(f)(s.roles)
        val offlineJudgesA =
          mapKeysTraversalUnsafe[String, Option[OfflineJudgingMode]].modifyF(f)(s.offlineJudges)
        (rolesA, offlineJudgesA).mapN { (roles, offlineJudges) =>
          s.copy(roles = roles, offlineJudges = offlineJudges)
        }
      }
    }

  val roundParticipants: Traversal[DebateRound, String] =
    new Traversal[DebateRound, String] {
      def modifyF[F[_]: Applicative](f: String => F[String])(s: DebateRound): F[DebateRound] =
        s match {
          case SimultaneousSpeeches(speeches) =>
            val speechesA =
              mapValuesTraversal[Int, DebateSpeech]
                .composeLens(DebateSpeech.speaker)
                .modifyF(f)(speeches)
            speechesA.map(SimultaneousSpeeches(_))
          case SequentialSpeeches(speeches) =>
            val speechesA =
              mapValuesTraversal[Int, DebateSpeech]
                .composeLens(DebateSpeech.speaker)
                .modifyF(f)(speeches)
            speechesA.map(SequentialSpeeches(_))
          case JudgeFeedback(distribution, feedback, endDebate) =>
            val feedbackA = DebateSpeech.speaker.modifyF(f)(feedback)
            feedbackA.map(JudgeFeedback(distribution, _, endDebate))
          case NegotiateEnd(votes) =>
            Applicative[F].pure(NegotiateEnd(votes))
          case OfflineJudgments(judgments) =>
            judgments
              .toList
              .traverse { case (judge, offlineJudgment) =>
                val judgeA = f(judge)
                val judgmentA =
                  OfflineJudgment
                    .judgments
                    .composeTraversal(monocle.function.all.each)
                    .composeLens(JudgeFeedback.feedback)
                    .composeLens(DebateSpeech.speaker)
                    .modifyF(f)(offlineJudgment)
                (judgeA, judgmentA).mapN(_ -> _)
              }
              .map(js => OfflineJudgments(js.toMap))
        }
    }

  val debateParticipants: Traversal[Debate, String] =
    new Traversal[Debate, String] {
      def modifyF[F[_]: cats.Applicative](f: String => F[String])(s: Debate): F[Debate] = {
        val setupA = debateSetupParticipants.modifyF(f)(s.setup)
        val roundsA =
          monocle
            .function
            .all
            .each[Vector[DebateRound], DebateRound]
            .composeTraversal(roundParticipants)
            .modifyF(f)(s.rounds)
        val feedbackA =
          mapKeysTraversalUnsafe[String, Feedback.SurveyResponse].modifyF(f)(s.feedback)
        (setupA, roundsA, feedbackA).mapN { (setup, rounds, feedback) =>
          s.copy(setup = setup, rounds = rounds, feedback = feedback)
        }
      }
    }

  def anonymizeParticipants(mapping: Map[String, String], debate: Debate): Debate =
    debateParticipants.modify(mapping)(debate)

}
