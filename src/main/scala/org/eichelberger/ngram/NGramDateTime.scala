package org.eichelberger.ngram

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeZone, DateTime}

object NGramDateTime extends SegmentLike[DateTime, String] {
  val StartPart = "_BOS_"

  val EndPart = "_EOS_"

  def emptyPart = ""

  def emptyWhole = new DateTime(Long.MinValue, DateTimeZone.forID("UTC"))

  val dtf = DateTimeFormat.forPattern("yyyyMMddHHmmssSSS")

  def combine(wholeSoFar: DateTime, part: String) =
    new DateTime((wholeSoFar.getMillis.toString + part).toLong, DateTimeZone.forID("UTC"))

  def disassemble(whole: DateTime): Iterator[String] =
    whole.getMillis.toString.map(_.toString).iterator
}

