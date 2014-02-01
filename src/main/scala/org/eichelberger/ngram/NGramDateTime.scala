package org.eichelberger.ngram

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeZone, DateTime}
import java.util.TimeZone

object SegmentDateTime extends SegmentLike[DateTime, String] {
  val StartPart = "_BOS_"

  val EndPart = "_EOS_"

  def emptyPart = ""

  def emptyWhole = new DateTime(0L, DateTimeZone.forID("UTC"))

  val dtf = DateTimeFormat.forPattern("yyyyMMddHHmmssSSS")

  def combine(wholeSoFar: DateTime, part: String) =
    new DateTime((wholeSoFar.getMillis.toString + part).toLong, DateTimeZone.forID("UTC"))

  def disassemble(whole: DateTime): Iterator[String] =
    whole.getMillis.toString.map(_.toString).iterator

  def useUTC {
    val tz = TimeZone.getTimeZone("UTC")
    TimeZone.setDefault(tz)
  }
  useUTC

  val UTC = DateTimeZone.forID("UTC")

  def dt2s(dt: DateTime): String = dtf.withZone(UTC).print(dt)

  def s2dt(s: String): DateTime = dtf.parseDateTime(s).withZone(UTC)

  def compare(a: String, b: String): Int = a.compareTo(b)
}

