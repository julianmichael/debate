package debate

import munit.CatsEffectSuite
import cats.effect.Resource
import cats.effect.Blocker
import cats.effect.IO
import debate.quality.QuALITYUtils
import java.nio.file.Paths

class AIDebateServiceTests extends CatsEffectSuite {
  val roles = List(Observer, Facilitator, Judge) ++ (0 to 3).map(Debater(_)).toList

  val dataPath = Paths.get("data")
  val qualityDataset = ResourceFixture(
    Resource
      .make(Blocker[IO].use(blocker => QuALITYUtils.readQuALITY(dataPath, blocker)))(_ => IO.unit)
  )

  val ambitionExampleQuote =
    "You grew up in the Age of Technology. Everybody was terribly excited about what could be done with machines-machines to blow up a city all at once, or fly around the world, or take a man to Mars. We have had our fill of-what is the word?-gadgets. Our machines serve us, and so long as they function right, we are satisfied to forget about them. Because this is the Age of Man. We are terribly interested in what can be done with people. Our scientists, like Swarts, are studying human rather than nuclear reactions. We are much more fascinated by the life and death of cultures than by the expansion or contraction of the Universe. With us, it is the people that are important, not gadgets."
  val ambitionExampleDebateText = """Debater A: Thank you, Judge. While Debater B claims that the reason they did not travel to the other planets is because they could not build rockets, I would like to present evidence to support my claim that they h
ad no desire to go.

<quote>"You grew up in the Age of Technology. Everybody was terribly excited about what could be done with machines-machines to blow up a city all at once, or fly around the world, or take a man to Mars. We have had our fill of-what is the word?-gadgets. Our machines serve us, and so long as they function right, we are satisfied to forget about them. Because this is the Age of Man. We are terribly interested in what can be done with people. Our scientists, like Swarts, are studying human rather than nuclear reactions. We are much more fascinated by the life and death of cultures than by the expansion or contraction of the Universe. With us, it is the people that are important, not gadgets."</quote>                                                                                                                                                     

This quote from the context clearly indicates that in their time, the focus shifted from technological advancements and exploration of outer space to the development and understanding of human culture and society. They became more interested in studying human reactions and the life and death of cultures, rather than the expansion of the universe.                                                                                                                                                                                           

Furthermore, the context shows that the protagonist, Maitland, longs to explore the other planets, but he is met with confusion and lack of interest from the people of that time. They do not understand his desire t
o travel to Mars or to see what lies beyond Earth. This further supports the claim that the people in this time period simply had no desire to go to the other planets.                                               
                                                                                                           
In conclusion, the evidence from the context clearly points to the fact that during this time, the people had no desire to travel to the other planets. Their focus shifted from technological advancements and explor
ation to the study of human culture and society. Therefore, I stand by my claim that the answer is They had no desire to go."""

  qualityDataset.test("longest common substring") { qualityDataset =>
    val ambition       = qualityDataset.find(_._2.title == "Ambition").get._2
    val ambitionTokens = Server.tokenizeStory(ambition.article)
    val quoteTokens    = Server.tokenizeStory(ambitionExampleQuote)
    val (indices, sequence) = AIDebateService
      .longestCommonVectorSubstring(ambitionTokens, quoteTokens)
    // val (indices, lengths, sequence) = AIDebateService
    // .longestCommonVectorSubstring(ambitionTokens, quoteTokens, "\\s+".r.matches)
    // println(s"Lengths: $lengths")
    println(s"Indices: $indices")
    println(s"Sequence: $sequence")

  // println(
  //   s"Story sequence prefix: " + ambitionTokens.drop(indices._1 - sequence.size - 5).take(10)
  // )
  // println(s"Quote sequence prefix: " + quoteTokens.drop(indices._2 - sequence.size - 5).take(10))

  }

  qualityDataset.test("recursively finding quotes") { qualityDataset =>
    val ambition       = qualityDataset.find(_._2.title == "Ambition").get._2
    val ambitionTokens = Server.tokenizeStory(ambition.article)
    val quoteTokens    = Server.tokenizeStory(ambitionExampleQuote)
    val segments = AIDebateService
      .recursivelyReconstructQuotes(ambitionTokens, quoteTokens, storyOffset = 0)
    // val (indices, lengths, sequence) = AIDebateService
    // .longestCommonVectorSubstring(ambitionTokens, quoteTokens, "\\s+".r.matches)
    // println(s"Lengths: $lengths")
    println(s"Speech segments: $segments")
  }
}
