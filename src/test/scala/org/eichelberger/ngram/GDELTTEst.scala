package org.eichelberger.ngram

import geomesa.utils.geohash.GeoHash
import org.joda.time.{DateTimeZone, DateTime}
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import java.io.BufferedInputStream
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class GDELTTest extends Specification {
  implicit val obj_NG_DT = SegmentDateTime
  implicit val obj_NG_GH = SegmentGeohashBinary
  implicit val obj_NG_S = SegmentString
  val windowSize = 5

  // self-closing wrapper for a GDELT subset
  def GDELT(sampleSize: Int = 100, dropSize: Int = 0) = new Iterable[Array[String]] {
    val reNumber = """^[+\-]?[0-9]+(\.[0-9]+)?"""
    def iterator = new Iterator[Array[String]] {
      // open the file
      val resource = getClass.getClassLoader.getResource("gdelt-50K.tsv")
      val src = Source.fromURL(resource, "Unicode")
      val lines = src.getLines.map(_.split("\t")).filter(_.size == 57).filter(f => f(53).matches(reNumber) && f(54).matches(reNumber))
      val netLines = if (sampleSize > 0) lines.drop(dropSize).take(sampleSize) else lines.drop(dropSize)
      var isClosed: Boolean = false
      def hasNext: Boolean = {
        if (! isClosed) {
          if (! netLines.hasNext) {
            src.close()
            isClosed = true
          }
        }
        !isClosed && netLines.hasNext
      }
      def next: Array[String] =
        if (hasNext) netLines.next
        else throw new Exception("File contains no more lines.")
    }
  }

  "GDELT test-sample file-accessor" should {
    "be able to read 100 entries more than once" in {
      GDELT(100, 0).size must be equalTo 100
      GDELT(100, 0).size must be equalTo 100
    }
  }

  case class BitsSpecification(gh: Int, dt: Int) {
    def size = gh + dt
  }

  case class Schema(row: BitsSpecification, colf: BitsSpecification, colq: BitsSpecification) {
    def encode(gh: GeoHash, dt: DateTime): String = {
      val ghs = gh.hash
      val dts = obj_NG_DT.dt2s(dt)
      println(s"[ENC] ghs=$ghs, dts=$dts")
      ghs.take(row.gh) + dts.take(row.dt) +
        ghs.drop(row.gh).take(colf.gh) + dts.drop(row.dt).take(colf.dt) +
        ghs.drop(row.gh + colf.gh).take(colq.gh) + dts.drop(row.dt + colf.dt).take(colq.dt)
    }
    def decode(enc: String): (GeoHash, DateTime) = {
      val ghs = enc.take(row.gh) +
        enc.drop(row.gh + row.dt).take(colf.gh) +
        enc.drop(row.size + colf.size).take(colq.gh)
      val dts = enc.drop(row.gh).take(row.dt) +
        enc.drop(row.size + colf.gh).take(colf.dt) +
        enc.drop(row.size + colf.size + colq.gh).take(colq.dt)
      println(s"ghs=$ghs, dts=$dts")
      (GeoHash(ghs, 35), obj_NG_DT.s2dt(dts))
    }
  }

// currently fails due to time-zone issues (go figure)

//  "simple schema" should {
//    "encode and decode correctly round-trip" in {
//      obj_NG_DT.useUTC
//
//      val schema = Schema(BitsSpecification(1, 8), BitsSpecification(3, 0), BitsSpecification(3, 9))
//      val gh = GeoHash(-78.0, 38.0, 35)
//      val dt = new DateTime(DateTimeZone.forID("UTC"))
//      val enc = schema.encode(gh, dt)
//      println(s"Encoded:  $enc <- $gh + $dt")
//      val dec = schema.decode(enc)
//
//      dec._1 must be equalTo gh
//      dec._2 must be equalTo dt
//    }
//  }

  "simple schema" should {
    "store and retrieve round-trip" in {
      val schema = Schema(BitsSpecification(1, 8), BitsSpecification(3, 0), BitsSpecification(3, 9))
      val gh = GeoHash(-78.0, 38.0, 35)
      val dt = new DateTime(DateTimeZone.forID("UTC"))
      val enc = schema.encode(gh, dt)

      val ngram = NGram.fromWhole(enc, windowSize)
      ngram must not beNull

      val encOut = ngram.sample
      encOut must be equalTo enc
    }
  }
}
