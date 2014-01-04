package org.eichelberger.ngram

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class NGramStringTest extends Specification {
  // allows the n-grams to find the string evidence
  implicit val obj = SegmentString

  "n-gram string" should {
    "count presentations correctly" in {
      val presentations = Set(
        "foo",
        "bar",
        "baz"
      )

      val ngram = presentations.foldLeft(NGram[String,String]())(
        (ngSoFar, presentation) => ngSoFar + presentation)

      ngram.presentationCount must be equalTo presentations.size
    }

    "recapitulate presentations from an unambiguous sample" in {
      val presentations = Set(
        "foo",
        "bar",
        "baz"
      )

      val ngram = presentations.foldLeft(NGram[String,String]())(
        (ngSoFar, presentation) => ngSoFar + presentation)

      val okay = (1 to 100).foldLeft(true)((okSoFar, i) => okSoFar && presentations.contains(ngram.sample))
      okay must be equalTo true
    }

    "honor the maximum presentation depth" in {
      val presentations = Set(
        "foo",
        "bar",
        "baz",
        "qux0r"
      )

      val ngram = presentations.foldLeft(NGram[String,String]())(
        (ngSoFar, presentation) => ngSoFar + presentation)

      ngram.maxDepthPresented must be equalTo 7
    }

    "count, and iterate through, terminals correctly" in {
      val presentations = Set(
        "foo",
        "bar",
        "baz",
        "qux0r"
      )

      val ngram = presentations.foldLeft(NGram[String,String](3))(
        (ngSoFar, presentation) => ngSoFar + presentation)

      ngram.prettyPrint()

      ngram.numTerminals must be equalTo 13

      val itr = ngram.sequenceIterator
      itr must not beNull;
      val itrSize = itr.foldLeft(0)((sizeSoFar, seq) => {
        println(s"[SEQ $sizeSoFar] $seq")
        sizeSoFar + 1
      })
      itrSize must be equalTo 13
    }

    "goof off for fun" in {
      // transitions range from a window size of 2 (nonsense) to 10 (recapitulation)
      val windowSize = 5
      val presentations = Set(
        "am i the only one here who recognizes how much fun n-grams can be?",
        "am i the only one here who misses vinyl?",
        "i find your lack of faith disturbing",
        "the only difference between me and a crazy person is that i am not crazy",
        "n-grams do not solve for snow"
      )

      val ngram = presentations.foldLeft(NGram[String,String](windowSize))(
        (ngSoFar, presentation) => ngSoFar + presentation)

      for (i <- 1 to 10) {
        println(s"[N-GRAM STRING SAMPLE]  ${ngram.sample}")
      }

      // degenerate condition
      1 must be equalTo 1
    }
  }
}
