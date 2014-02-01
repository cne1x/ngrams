package org.eichelberger.ngram

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class NGramStringTest extends Specification {
  // allows the n-grams to find the string evidence
  implicit val obj = SegmentString

  "n-gram string" should {
//    "count presentations correctly" in {
//      val presentations = Set(
//        "foo",
//        "bar",
//        "baz"
//      )
//
//      val ngram = presentations.foldLeft(NGram[String,String]())(
//        (ngSoFar, presentation) => ngSoFar + presentation)
//
//      ngram.numPresentations must be equalTo presentations.size
//    }
//
//    "recapitulate presentations from an unambiguous sample" in {
//      val presentations = Set(
//        "foo",
//        "bar",
//        "baz"
//      )
//
//      val ngram = presentations.foldLeft(NGram[String,String]())(
//        (ngSoFar, presentation) => ngSoFar + presentation)
//
//      val okay = (1 to 100).foldLeft(true)((okSoFar, i) => okSoFar && presentations.contains(ngram.sample))
//      okay must be equalTo true
//    }
//
//    "honor the maximum presentation depth" in {
//      val presentations = Set(
//        "foo",
//        "bar",
//        "baz",
//        "qux0r"
//      )
//
//      val ngram = presentations.foldLeft(NGram[String,String]())(
//        (ngSoFar, presentation) => ngSoFar + presentation)
//
//      ngram.maxDepthPresented must be equalTo 7
//    }
//
//    "count, and iterate through, terminals correctly" in {
//      val presentations = Set(
//        "foo",
//        "bar",
//        "baz",
//        "qux0r"
//      )
//
//      val ngram = presentations.foldLeft(NGram[String,String](3))(
//        (ngSoFar, presentation) => ngSoFar + presentation)
//
//      ngram.prettyPrint()
//
//      ngram.numTerminals must be equalTo 13
//
//      val itr = ngram.sequenceIterator
//      itr must not beNull;
//      val itrSize = itr.foldLeft(0)((sizeSoFar, seq) => {
//        println(s"[SEQ $sizeSoFar] $seq")
//        sizeSoFar + 1
//      })
//      itrSize must be equalTo ngram.numTerminals.toInt
//    }
//
//    "scale counts correctly" in {
//      val presentations = Set(
//        "foo",
//        "food",
//        "baz",
//        "bazzer"
//      )
//
//      val ngram = presentations.foldLeft(NGram[String,String](3))(
//        (ngSoFar, presentation) => ngSoFar + presentation)
//      val ngramScaled = ngram * 10
//
//
//      println("[RAW NGRAM]")
//      ngram.prettyPrint()
//      println("[SCALED NGRAM]")
//      ngramScaled.prettyPrint()
//
//      // test
//      ngramScaled.toString must be equalTo "'':3:160=<'_BOS_':3:40=<'b':3:20=<'a':3:20=<>>, 'f':3:20=<'o':3:20=<>>>, 'a':3:20=<'z':3:20=<'_EOS_':3:10=<>, 'z':3:10=<>>>, 'b':3:20=<'a':3:20=<'z':3:20=<>>>, 'e':3:10=<'r':3:10=<'_EOS_':3:10=<>>>, 'f':3:20=<'o':3:20=<'o':3:20=<>>>, 'o':3:30=<'d':3:10=<'_EOS_':3:10=<>>, 'o':3:20=<'_EOS_':3:10=<>, 'd':3:10=<>>>, 'z':3:20=<'e':3:10=<'r':3:10=<>>, 'z':3:10=<'e':3:10=<>>>>"
//    }

    "estimate lexical distances correctly" in {
      // presentation -> count
      val data = Map(
        "foo" -> 3,
        "bar" -> 2
      )
      val windowSize = 3

      val ngram = data.foldLeft(NGram[String,String](windowSize))(
        (ngSoFar, kv) => {
          val (presentation, count) = kv
          val addNG = NGram.fromWhole(presentation, windowSize) * count
          ngSoFar + addNG
        })

      println("[LEXICAL BASIS]")
      ngram.prettyPrint()

      def testPos(s: String, expected: Double) = {
        val idx = ngram.getRelativePosition(s)
        println(s"  [LEXICAL INDEX] $s -> $idx")
        idx must be equalTo expected
      }

      def testDiff(a: String, b: String, expected: Double) = {
        val diff = ngram.getLexicalDifference(a, b)
        println(s"  [LEXICAL DISTANCE] ($a, $b) -> $diff")
        diff must be equalTo expected
      }

      testPos("foo", 3.5)
      testPos("bar", 1.0)
      testPos("cat", 2.0)
      testPos("aaa", 0.0)
      testPos("zzz", 5.0)

      testDiff("bar", "cat", 0.2)
      testDiff("zzz", "aaa", 1.0)
      testDiff("foo", "foo", 0.0)
      testDiff("cat", "cat", 0.0)
    }
  }
}
