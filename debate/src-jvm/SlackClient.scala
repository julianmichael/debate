package debate

import cats.implicits._
import io.circe.Json

import org.http4s.client.Client
import org.http4s._
import org.http4s.circe._
import cats.effect.IO

case class SlackClient(httpClient: Client[IO], token: String) {

  def lookupByEmail(email: String) = httpClient
    .expect[Json](
      Request[IO](
        method = Method.GET,
        uri = Uri
          .fromString("https://slack.com/api/users.lookupByEmail")
          .toOption
          .get
          .withQueryParam("email", email),
        headers = Headers.of(Header("Authorization", s"Bearer $token"))
      )
    )
    .map { responseJson =>
      responseJson.asObject.get("user").get.asObject.get("id").get.as[String].toOption.get
    }

  def postMessage(recipientId: String, message: String) =
    httpClient
      .expect[Json](
        Request[IO](
          method = Method.POST,
          uri = Uri
            .fromString("https://slack.com/api/chat.postMessage")
            .toOption
            .get
            .withQueryParam("channel", recipientId)
            .withQueryParam("text", message),
          headers = Headers.of(Header("Authorization", s"Bearer $token"))
        )
      )
      .void

  def sendMessage(profiles: Map[String, Profile], debater: String, msg: String) = {

    val notify = profiles
      .get(debater)
      .flatMap(_.slackEmail)
      .traverse(lookupByEmail)
      .flatMap(_.traverse_(id => postMessage(id, msg)))

    notify.recoverWith { case e: Throwable =>
      IO {
        println(s"Slack notification failed for $debater")
        // println(s"Email: $email")
        println(s"Message: ${e.getMessage()}")
        println(s"Stack trace: ")
        e.printStackTrace()
      }
    }

  }

}
