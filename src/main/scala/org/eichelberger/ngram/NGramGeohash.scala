package org.eichelberger.ngram

import geomesa.utils.geohash.GeoHash

// example implementation of a GeoHash (per bit) segment
object SegmentGeohashBinary extends SegmentLike[GeoHash, String] {
  val StartPart = "_BOS_"
  val EndPart = "_EOS_"
  val MaxBits = 63
  def emptyPart = ""
  def emptyWhole = GeoHash(0.0, 0.0, 0)
  def binaryStringToGeohash(binaryString: String): GeoHash = {
    val netString = binaryString.take(MaxBits)
    val bitsOn = netString.zipWithIndex.filter(_._1 == '1')
    val bitSet = scala.collection.immutable.BitSet(bitsOn.map(_._2):_*)
    GeoHash(bitSet, netString.length)
  }
  def combine(wholeSoFar: GeoHash, part: String) =
    binaryStringToGeohash(wholeSoFar.toBinaryString + Option(part).getOrElse(""))
  def disassemble(whole: GeoHash): Iterator[String] = whole.toBinaryString.iterator.map(_.toString)

  def compare(a: String, b: String): Int = a.compareTo(b)
}
