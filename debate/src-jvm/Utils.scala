package debate

import debate.singleturn.SingleTurnDebateQuestion
import debate.quality.QuALITYStory
import cats.data.ValidatedNec
import cats.implicits._

trait UtilsPlatformExtensions {

  def charReplacementDistance(x: String, y: String): Int = {
    val lengthDiff = math.abs(x.size - y.size)
    val editDiff   = x.zip(y).filter(Function.tupled(_ != _)).size
    lengthDiff + editDiff
  }

  def transformQualityQ(q: String) =
    q.replaceAll("’", "'").replaceAll("“", "\"").replaceAll("”", "\"").replaceAll("\u00A0", "").trim
  // .replaceAll("\n", "")

  def transformDebateQ(q: String) =
    q.replaceAll("<U\\+FFFD>", "").trim // .replaceAll("\"", "…").trim

  def identifyQualityMatches(
    qualityDataset: Map[String, QuALITYStory],
    singleTurnDebateDataset: Map[String, Vector[SingleTurnDebateQuestion]]
  ): Map[String, Set[String]] = qualityDataset.map { case (articleId, story) =>
    val singleTurnQs = singleTurnDebateDataset.get(articleId).combineAll.toSet
    articleId ->
      singleTurnQs
        .toVector
        .flatMap(singleTurnQ =>
          story
            .questions
            .find { case (_, question) =>
              transformQualityQ(question.question) == transformDebateQ(singleTurnQ.questionText)
            }
            .map(_._1)
        )
        .toSet
  }

  // get QuALITY question IDs present in the single-turn debate dataset
  def validateQualityMatches(
    qualityDataset: Map[String, QuALITYStory],
    singleTurnDebateDataset: Map[String, Vector[SingleTurnDebateQuestion]]
  ): ValidatedNec[String, Map[String, Set[String]]] =
    Option(singleTurnDebateDataset.keySet -- qualityDataset.keySet)
      .filter(_.nonEmpty)
      .map(ids =>
        s"Single-turn debate data contains passage IDs missing from QuALITY: ${ids.mkString(", ")}\n"
      )
      .toInvalidNec(()) *>
      qualityDataset
        .toVector
        .traverse { case (articleId, story) =>
          val singleTurnQs = singleTurnDebateDataset.get(articleId).combineAll.toSet
          singleTurnQs
            .toVector
            .traverse(singleTurnQ =>
              story
                .questions
                .find { case (_, question) =>
                  transformQualityQ(question.question) == transformDebateQ(singleTurnQ.questionText)
                }
                .map(_._1)
                .toValidNec {
                  val debateQ = transformDebateQ(singleTurnQ.questionText)
                  val qs      = story.questions.values.map(_.question).map(transformQualityQ)
                  val closestQs = qs
                    .zip(qs.map(q => charReplacementDistance(q, debateQ)))
                    .filter(_._2 <= 15)
                  if (closestQs.nonEmpty) {
                    val closestQualityQ = closestQs.minBy(_._2)._1
                    s"Can't find question: `$debateQ`\n${debateQ.takeRight(3).map(_.toInt)}\nClosest QuALITY Q: `${transformQualityQ(
                        closestQualityQ
                      )}`\n${closestQualityQ.takeRight(3).map(_.toInt)}\n"
                  } else {
                    ""
                  }
                }
            )
            .map(_.toSet)
            .map(articleId -> _)
        }
        .map(_.toMap)
}
