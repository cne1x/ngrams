package org.eichelberger.ngram

import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import scala.io.Source

/*
 * Texts used in these tests have been extracted from the Project Gutenberg versions.
 * There are two reasons why entire files have not been used:
 * 1.  In some cases, the texts were too long for unit tests;
 * 2.  The preamble and codas were unsuitable for word-frequency tests.
 *
 * These sub-texts are not meant to misrepresent any intellectual property.
 * Please see http://www.gutenberg.org/wiki/Gutenberg:Terms_of_Use
 * and be thankful that such a resource exists for freely available texts.
 */
@RunWith(classOf[JUnitRunner])
class SegmentSentenceTest extends Specification {
  implicit val obj = SegmentSentence

  // 1063:  Fall of the House of Usher
  def getLocalGutenbergText(work: String): String = {
    val url = getClass.getClassLoader.getResource(s"texts/$work.txt")
    val src = Source.fromURL(url)

    val lines = src.getLines()
    val result = lines.toList.mkString(" ")

    src.close()
    result
  }

  def toSentences(text: String): List[String] = {
    val r0 = text.split("""((\.)|!|(\?))\s""").toList

    val reJoin = """.*(mr|ms|mrs|etc|eg|(e\.g)|ie|(i\.e))$"""

    val r1_ = r0.foldLeft(("", List[String]()))((t, sentence) => {
      val endsWell = t._1.toLowerCase.matches(reJoin)
      val startChar = sentence.take(1).toString
      val startsWell = startChar.toUpperCase == startChar
      if (endsWell && startsWell) (t._1.trim + ". " + sentence.trim, t._2)
      else (sentence, t._2 ++ List(t._1))
    })
    val r1 = r1_._2 ++ List(r1_._1)

    r1.map(_.trim).filter(_.length > 0)
  }

  "Sentence extractor" should {
    "do a reasonable job" in {
      val text = "The quick brown fox jumped over Mr. White.  The end.  Ms. Scarlet looked on, e.g., appalled."
      val sentences = toSentences(text)

      sentences.foreach(s => println(s"[SENTENCE] $s"))

      sentences.size must be equalTo(3)
    }
  }

  "Gutenberg extractor" should {
    "extract the _Cask of Amontillado_" in {
      val text = getLocalGutenbergText("CaskOfAmontillado")
      text.length must be equalTo 13025
    }
  }

  "Random-text generator" should {
    "generate random sentences" in {
      val sources = List[String](
        "CaskOfAmontillado"
        ,"HuckFinn-Chapter1"
        ,"PrideAndPrejudice-Chapter1"
        ,"Frankenstein-Chapter2"
      )

      val windowSize = 4
      val emptyNGram = NGram[String,String](windowSize)
      val ngram = sources.foldLeft(emptyNGram)((ngOuter, source) => {
        val text = getLocalGutenbergText(source)
        val sentences = toSentences(text)
        sentences.foldLeft(ngOuter)((ngInner, sentence) => {
          val ng = ngInner + sentence
          //println(s"[SENTENCE] <$sentence>")
          ng.validate
          ng
        })
      })

      //ngram.getLastNode(List(obj.StartPart)).prettyPrint()

      //ngram.prettyPrint()

      println(s"Ngram window size:  ${ngram.windowSize}")

      (1 to 100).foreach(i => {
        println(s"[RANDOM] ${ngram.sample}")
      })

      1 must be equalTo 1
    }
  }
}
