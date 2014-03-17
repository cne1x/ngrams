package org.eichelberger.ngram

import geomesa.utils.geohash.GeoHash
import org.joda.time.{DateTimeZone, DateTime}
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import java.io.BufferedInputStream
import scala.io.Source
import org.joda.time.format.DateTimeFormat

/**
 * The GDELT data set -- see gdeltproject.org -- contains geo-time data
 * extracted from plain-text news stories.  To test some of the n-grams,
 * we have aggregated the data so that each unique 30-bit GeoHash'ed
 * location gets one line in our summary file, formatted as follows:
 *
 *      YYYYMMDD     GeoHash     count
 *
 * The date is the first with which the location was ever associated with
 * an event in GDELT.  As you would expect, even though there are more
 * news sources read as time went by, there are fewer and fewer unique
 * locations associated with any single date as the dates grow later.
 *
 * The purpose of such a format is to allow us to reconstruct (at least
 * at 30-bit precision) the progression of summary n-grams built from
 * GDELT.
 */

@RunWith(classOf[JUnitRunner])
class GDELTTest extends Specification {
  implicit val obj_NG_DT = SegmentDateTime
  implicit val obj_NG_GH = SegmentGeohashBinary
  implicit val obj_NG_S = SegmentString
  val windowSize = 5

  val gdeltDTF = DateTimeFormat.forPattern("yyyyMMdd")

  // a single entry from this summary file of GDELT data
  object GdeltEntry {
    // alternate constructor from a single, unparsed string
    def apply(unparsed: String): GdeltEntry = {
      val fields = unparsed.split("\t")
      apply(fields)
    }
    // alternate constructor from a single, unparsed string
    def apply(fields: Array[String]): GdeltEntry = {
      require(fields.size == 3, s"Expected 3 tab-separated fields; found ${fields.size}")
      val dt = gdeltDTF.parseDateTime(fields(0))
      val gh = GeoHash(fields(1))
      val num = fields(2).toLong

      GdeltEntry(dt, gh, num)
    }
  }
  case class GdeltEntry(when: DateTime, where: GeoHash, count: Long)

  // self-closing wrapper for a GDELT subset
  def GDELT(sampleSize: Int = 100, dropSize: Int = 0) = new Iterable[GdeltEntry] {
    val reNumber = """^[+\-]?[0-9]+(\.[0-9]+)?"""
    val reDate = """^[12][09]\d\d[01]\d[0123]\d$"""
    def iterator = new Iterator[GdeltEntry] {
      // open the file
      val resource = getClass.getClassLoader.getResource("gdelt-firsts.tsv")
      val src = Source.fromURL(resource)
      val lines = src.getLines.map(_.split("\t")).filter(_.size == 3).filter(f => f(0).matches(reDate))
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
      def next: GdeltEntry =
        if (hasNext) GdeltEntry(netLines.next)
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

  def recommendSchema(ngGH: NGram[GeoHash, String], ngDT: NGram[String, String], threshold: Double = 0.05): Schema = {
    val ghScores = (5 to 35 by 5).map(ghb => {
      val r = ngGH.getMostFrequent(ghb)
      println(s"  GH $ghb at ${new DateTime()} = $r")
      r
    })
    val dtScores = (1 to 17).map(dtb => {
      val r = ngDT.getMostFrequent(dtb)
      println(s"  DT $dtb at ${new DateTime()} = $r")
      r
    })

    println(s"ghScores:  ${ghScores.map(_.toString).mkString(", ")}")
    println(s"dtScores:  ${dtScores.map(_.toString).mkString(", ")}")

    val scores: List[(List[String], List[String], Double, Int, Int)] = (for (
      ghb <- 1 to ghScores.size;
      dtb <- 1 to dtScores.size;
      ghScore = ghScores(ghb - 1);
      dtScore = dtScores(dtb - 1);
      scored = (ghScore._1, dtScore._1, ghScore._2 * dtScore._2, ghb, dtb)
    ) yield scored).sortWith((a,b) => a._3 < b._3).toList

    val qualifiers = scores.filter(_._3 <= threshold)

    qualifiers.takeRight(10).foreach(s => {
      val binary: String = s._1.tail.mkString
      val ghs = obj_NG_GH.binaryStringToGeohash(binary)
      val dt = s._2
      val score = s._3.formatted("%1.4f")

      println(s"[QUALIFYING] $score = ($ghs, $dt)")
    })

    val best = qualifiers.last
    val ghb: Int = best._4
    val dtb: Int = best._5

    //@TODO code
    val row = BitsSpecification(ghb, dtb)
    val colf = BitsSpecification(math.min(2, 7 - ghb), 0)
    val colq = BitsSpecification(math.max(0, 7 - row.gh - colf.gh), math.max(0, 17 - row.dt - colf.dt))
    Schema(row, colf, colq)
  }

  "independent n-grams" should {
    "recommend index-schema" in {
      val windowSize = 10
      val engGH = NGram[GeoHash, String](windowSize)
      val engDT = NGram[String, String](windowSize)

      val pair = GDELT(1000, 0).foldLeft((engGH, engDT))((t, entry) => t match { case (ngGHSoFar, ngDTSoFar) =>
        (
          ngGHSoFar + entry.where,
          ngDTSoFar + obj_NG_DT.dt2s(entry.when)
        )
      })

      val (ngGH, ngDT) = pair

      val schema = recommendSchema(ngGH, ngDT, 0.01)
      println(s"Schema:  $schema")

      schema must be equalTo Schema(BitsSpecification(2,17),BitsSpecification(2,0),BitsSpecification(3,0))
    }
  }
}
