package debate.quality

import io.circe.generic.extras.ConfiguredJsonCodec
import io.circe.generic.extras.Configuration
import monocle.macros.Lenses
import io.circe.generic.JsonCodec

// case class QuALITYDataset(
//   stories: Map[String, QuALITYStory],
// )

// @JsonCodec sealed trait QuALITYSplit
// object QuALITYSplit {
//   case object Train extends 
// }

@JsonCodec case class QuALITYStory(
  articleId: String,
  source: String,
  title: String,
  year: Option[Int],
  author: String,
  topic: String,
  article: String,
  url: String,
  license: String,
  questions: Map[String, QuALITYQuestion]
) {
  def merge(that: QuALITYStory): Either[IllegalArgumentException, QuALITYStory] = {
    def getIfEqual[A](fieldName: String, thisValue: A, thatValue: A) = {
      if(thisValue == thatValue) Right(thisValue) else {
        Left(new IllegalArgumentException(s"Unequal $fieldName:\n\t$thisValue\n!=\t$thatValue"))
      }
    }
    for {
      articleId <- getIfEqual("articleId", this.articleId, that.articleId)
      source <- getIfEqual("source", this.source, that.source)
      title <- getIfEqual("title", this.title, that.title)
      year <- getIfEqual("year", this.year, that.year)
      author <- getIfEqual("author", this.author, that.author)
      topic <- getIfEqual("topic", this.topic, that.topic)
      article <- getIfEqual("article", this.article, that.article)
      url <- getIfEqual("url", this.url, that.url)
      license <- getIfEqual("license", this.license, that.license)
    } yield QuALITYStory(
      articleId, source, title, year, author,
      topic, article, url, license,
      this.questions ++ that.questions
    )
  }
}

@JsonCodec case class QuALITYQuestion(
  split: String,
  setUniqueId: String,
  batchNum: Int,
  writerId: Int,
  questionUniqueId: String,
  question: String,
  options: Vector[String],
  difficult: Boolean
)

// only putting this silly object here because scala 2 doesn't allow vals not in objects/etc.
// and I don't want to have to put this implicit in another file
// and I assume I need to import it for the annotation to work
private object SillyObject {
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
}
import SillyObject.config

@ConfiguredJsonCodec @Lenses case class QuALITYInstance(
  articleId: String,
  setUniqueId: String,
  batchNum: Int,
  writerId: Int,
  source: String,
  title: String,
  year: Option[Int],
  author: String,
  topic: String,
  article: String,
  questions: Vector[QuALITYQuestionInstance],
  url: String,
  license: String
) {
  def toStory(split: String) = QuALITYStory(
      articleId = articleId,
      source = source,
      title = title,
      year = year,
      author = author,
      topic = topic,
      article = article,
      url = url,
      license = license,
      questions = questions.view.map { q =>
        val difficult = q.difficult match {
          case 0 => false
          case 1 => true
          case _ => throw new IllegalArgumentException(s"Question difficulty must be 0 or 1. Found: ${q.difficult}")
        }
        q.questionUniqueId -> QuALITYQuestion(
          split = split,
          setUniqueId = setUniqueId,
          batchNum = batchNum,
          writerId = writerId,
          questionUniqueId = q.questionUniqueId,
          question = q.question,
          options = q.options,
          difficult = difficult
        )
      }.toMap
    )
}
object QuALITYInstance

@ConfiguredJsonCodec @Lenses case class QuALITYQuestionInstance(
  questionUniqueId: String,
  question: String,
  options: Vector[String],
  difficult: Int
)
object QuALITYQuestionInstance
