package debate

import debate.singleturn.SingleTurnDebateQuestion
import debate.quality.QuALITYStory
import cats.data.ValidatedNec
import cats.implicits._

trait UtilsPlatformExtensions {
  // get QuALITY question IDs present in the single-turn debate dataset
  def identifyQualityMatches(
    qualityDataset: Map[String, QuALITYStory],
    singleTurnDebateDataset: Map[String, Vector[SingleTurnDebateQuestion]]
  ): ValidatedNec[String, Map[String, Set[String]]] =
    Option(singleTurnDebateDataset.keySet -- qualityDataset.keySet)
      .filter(_.nonEmpty)
      .map(ids =>
        s"Single-turn debate data contains passage IDs missing from QuALITY: ${ids.mkString(", ")}"
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
                  question.question.replaceAll("’", "'") == singleTurnQ.questionText
                }
                .map(_._1)
                .toValidNec(s"Can't find question: ${singleTurnQ.questionText}")
            )
            .map(_.toSet)
            .map(articleId -> _)
        }
        .map(_.toMap)

}
