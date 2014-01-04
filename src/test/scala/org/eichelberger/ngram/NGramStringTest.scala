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
      itrSize must be equalTo ngram.numTerminals.toInt
    }

    "build a square, symmetric sequence association matrix correctly, and reduce it" in {
      val presentations = Set(
        "foo",
        "bar",
        "baz",
        "qux0r",
        "barsoom",
        "barstool",
        "toadstool",
        "barfood"
      )

      val ngram = presentations.foldLeft(NGram[String,String](3))(
        (ngSoFar, presentation) => ngSoFar + presentation)
      val m = ngram.numTerminals.toInt

      // build the association matrix
      val itr = ngram.sequenceIterator
      itr must not beNull;
      val matrix = itr.foldLeft(List.empty[List[Int]])((mSoFar, seq) => {
        val assoc = ngram.sequenceAssociationCounts(seq)
        mSoFar ++ List(assoc)
      })

      // validate the association matrix
      println(s"[ASSOCIATION MATRIX] $matrix")
      for (row <- 0 until m; col <- 0 until m) {
        matrix(row)(col) must be equalTo matrix(col)(row)
      }
      matrix.size must be equalTo m

      // remember the initial row sums
      val sums: Seq[Int] = (0 until m).map(row => matrix(row).sum)

      // identify those rows whose sum is greater than 1
      // (that is, the probe is not subsumed in another sequence)
      val nontrivialSums: List[(Int, Int)] = sums.zipWithIndex.
        sortWith((a,b) => a._1 > b._1).
        filter(_._1 > 1).
        toList

      // remove as many of these non-trivial probes as possible, in order
      val (finalSums, finalIdxs) = nontrivialSums.foldLeft((sums, (0 until m).toSet))((soFar, sumPair) => {
        val (sumsSoFar, idxsSoFar) = soFar
        val (_, rowIdx) = sumPair
        val rowValues: List[Int] = matrix(rowIdx)

        val newSums: Seq[Int] = (0 until m).map(i => sumsSoFar(i) - rowValues(i))
        val canRemoveThisIdx: Boolean = !newSums.exists(_ < 1)

        println(s"[consider reducing] $rowIdx : $newSums -> $canRemoveThisIdx")

        if (canRemoveThisIdx) (newSums, idxsSoFar - rowIdx)
        else (sumsSoFar, idxsSoFar)
      })
      println(s"[INITIAL SUMS] $sums")
      println(s"[REDUCED SUMS] $finalSums")
      println(s"[REDUCED INDEXES] ${finalIdxs.toList.sorted}")

      //@TODO move this out, and make it more abstract
      case class Probe(seq: Seq[String]) {
        override def toString: String =
          if (seq.size > 0) seq.mkString("['", "', '", "']")
          else "[]"

        val PartLead = "<"
        val PartSeparator = "~"
        val PartClose = ">"

        lazy val compString: String = seq.mkString(PartLead, PartClose + PartSeparator + PartLead, PartClose)

        def apply(x: String): Boolean = {
          val xSeq: Seq[String] = obj.decompose(x).toList
          val xStr: String = xSeq.mkString(PartLead, PartClose + PartSeparator + PartLead, PartClose)

          //println(s"[probing] $xStr contains $compString = ${xStr.indexOf(compString)}")

          //@TODO make this search less sucky
          xStr.indexOf(compString) >= 0
        }
      }

      // list the final probe-set
      val allProbes: List[Probe] = ngram.sequenceIterator.toList.map(seq => Probe(seq))
      finalIdxs.foreach { idx =>
        println(s"[PROBE] ${allProbes(idx)}")
      }

      // list the probe-result-vector for each of the input samples
      presentations.foreach { input =>
        val vector: Seq[String] = finalIdxs.toSeq.map(idx => {
          val probe = allProbes(idx)
          val probeResult = probe(input)
          //println(s"[single probe result] #$idx $probe ($input) = $probeResult")

          if (probeResult) "1"
          else "0"
        })
        println(s"[PROBE VECTOR] $input -> ${vector.mkString(" ")}")
      }

      //@TODO put a real condition here
      1 must be equalTo 1
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
