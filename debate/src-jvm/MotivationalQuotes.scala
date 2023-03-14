package debate

object MotivationalQuotes {
  val rand = new scala.util.Random()

  case class Quotes(quotes: List[String]) {
    def sample() = quotes(rand.nextInt(quotes.size))
  }
  object Quotes {
    def apply(qs: String*): Quotes = new Quotes(qs.toList)
  }

  val yourTurn = Quotes(
    "The time is now. The time is always now.",
    "You will speak up. You will show up. You will stand up.",
    "Let your light shine. Shine within you so that it can shine on someone else."
  )

  val newJudging = Quotes(
    "Only make decisions that support your self-image, self-esteem, and self-worth.",
    "Check your ego at the door and check your gut instead. Every right decision I have ever made has come from my gut.",
    "Follow your instincts. That's where true wisdom manifests itself.",
    "Don't be afraid to go with your gut. It's there for a reason, and it's usually right.",
    "I rely far more on gut instinct than researching huge amounts of statistics.",
    "If you let it, the noise of the world will drown out the voice of God, which is your intuition."
  )

  val newMaterial = Quotes(
    "The more you know, the better you do.",
    "You are not your circumstances. You are your possibilities. If you know that, you can do anything.",
    "Luck is preparation meeting the moment of opportunity.",
    "You face the biggest challenge of all: to have the courage to seek your big dream regardless of what anyone says."
  )

  val youveTied = Quotes(
    "You get a car! They get a car! Everyone gets a car!",
    "Wellness to me means all things in balance. And balance doesn't mean all things are equal or at peace at all times."
  )

  val youveWon = Quotes(
    "The more you praise and celebrate your life, the more there is in life to celebrate.",
    "You get in life what you have the courage to ask for."
  )

  val youveLost = Quotes(
    "Turn your wounds into wisdom.",
    "Failure is just an experience.",
    "Sometimes winning is just finishing.",
    "Doing what is right, fair, or honorable is more important than winning or losing.",
    "The key to realizing a dream is to focus not on success but on significance—and then even the small steps and little victories along your path will take on greater meaning.",
    "Challenges are gifts that force us to search for a new center of gravity. Don't fight them. Just find a new way to stand."
  )
}
