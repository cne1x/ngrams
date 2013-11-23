package org.eichelberger.ngram

// example implementation of a string segment
object SegmentString extends SegmentLike[String, String] {
  val StartPart = "_BOS_"
  val EndPart = "_EOS_"
  def emptyPart = ""
  def emptyWhole = ""
  def combine(wholeSoFar: String, part: String) =
    Option(wholeSoFar).getOrElse("") + Option(part).getOrElse("")
  override def assemble(parts: Iterator[String]): String = parts.mkString
  def disassemble(whole: String): Iterator[String] = whole.iterator.map(_.toString)
}

