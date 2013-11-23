package org.eichelberger.ngram

object SegmentSentence extends SegmentLike[String,String] {
  val StartPart = "_BOS_"
  val EndPart = "_EOS_"
  def emptyPart = ""
  def emptyWhole = ""
  def combine(wholeSoFar: String, part: String) =
    (Option(wholeSoFar).getOrElse("") + " " + Option(part).getOrElse("")).trim
  override def assemble(parts: Iterator[String]): String = parts.mkString(" ")
  def disassemble(whole: String): Iterator[String] = {
    whole.replaceAll("([;.\\-)(,?:\"!])", " $1 ").toUpperCase.trim.split("\\s+").iterator
  }
}
