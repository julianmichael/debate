package debate

import io.circe.generic.JsonCodec

@JsonCodec
case class Profile(name: String, slackEmail: Option[String])
