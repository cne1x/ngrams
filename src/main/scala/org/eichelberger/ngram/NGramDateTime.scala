package org.eichelberger.ngram

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTimeZone, DateTime}

object NGramDateTime extends SegmentLike[DateTime, String] {
  val dtf = DateTimeFormat.forPattern("yyyyMMddHHmmss.SSS")

  val StartPart = "_BOS_"

  val EndPart = "_EOS_"

  def emptyPart = ""

  def emptyWhole = new DateTime(Long.MinValue, DateTimeZone.forID("UTC"))

  def combine(wholeSoFar: DateTime, part: String) = {
    part.split("=") match {
      case Array("y0", y0) =>
        val oldYear = wholeSoFar.getYear
        val newYear = (oldYear % 1000) + (1000 * y0.toInt)
        wholeSoFar.withYear(newYear)
      case _ =>
        throw new Exception(s"Invalid date-time portion:  '$part'")
    }
  }

  def disassemble(whole: DateTime): Iterator[String] = {
    val dts = dtf.print(whole)

  }
//  def emptyPart = ""
//  def emptyWhole = GeoHash(0.0, 0.0, 0)
//  def binaryStringToGeohash(binaryString: String): GeoHash = {
//    val netString = binaryString.take(MaxBits)
//    val bitsOn = netString.zipWithIndex.filter(_._1 == '1')
//    val bitSet = scala.collection.immutable.BitSet(bitsOn.map(_._2):_*)
//    GeoHash(bitSet, netString.length)
//  }
//  def combine(wholeSoFar: GeoHash, part: String) =
//    binaryStringToGeohash(wholeSoFar.toBinaryString + Option(part).getOrElse(""))
//  def disassemble(whole: GeoHash): Iterator[String] = whole.toBinaryString.iterator.map(_.toString)
}

