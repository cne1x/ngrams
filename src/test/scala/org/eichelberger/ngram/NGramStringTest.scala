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

      def testPos(s: String, expected: Long, onlyUnique: Boolean = true) = {
        val idx = ngram.getRelativePosition(s, onlyUnique)
        println(s"  [LEXICAL INDEX] $s -> $idx")
        idx must be equalTo expected
      }

      def testDiff(a: String, b: String, expected: Double, onlyUnique: Boolean) = {
        val diff = ngram.getLexicalDifference(a, b, onlyUnique)
        println(s"  [LEXICAL DISTANCE] ($a, $b) -> $diff")
        diff must be equalTo expected
      }

      testPos("foo", 7, false)
      testPos("bar", 2, false)
      testPos("cat", 4, false)
      testPos("aaa", 0, false)
      testPos("zzz", 10, false)

      testDiff("foo", "bar", 0.50, false)
      testDiff("bar", "cat", 0.20, false)
      testDiff("zzz", "aaa", 1.00, false)
      testDiff("foo", "foo", 0.00, false)
      testDiff("cat", "cat", 0.00, false)

      testPos("foo", 3, true)
      testPos("bar", 1, true)
      testPos("cat", 2, true)
      testPos("aaa", 0, true)
      testPos("zzz", 4, true)

      testDiff("foo", "bar", 0.50, true)
      testDiff("bar", "cat", 0.25, true)
      testDiff("zzz", "aaa", 1.00, true)
      testDiff("foo", "foo", 0.00, true)
      testDiff("cat", "cat", 0.00, true)
    }
  }
}
