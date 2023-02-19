package debate

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

}
