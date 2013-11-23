package org.eichelberger.ngram

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.joda.time.{DateTimeZone, DateTime}

@RunWith(classOf[JUnitRunner])
class NGramDateTimeTest extends Specification {
  implicit val obj = SegmentDateTime
  val windowSize = 5

  "empty ngram" should {
    "be non-null" in {
      val ngram = NGram[DateTime,String](windowSize)
      ngram must not beNull
    }
  }

  "single entry" should {
    "be perfectly recoverable" in {
      val dtIn = new DateTime().withZone(DateTimeZone.forID("UTC"))

      val ngram = NGram.fromWhole(dtIn, windowSize)
      ngram must not beNull

      ngram.prettyPrint()

      val dtOut = ngram.sample
      dtOut must be equalTo dtIn
    }
  }
}
