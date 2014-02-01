package org.eichelberger.ngram

/**
 * The entire point of n-grams is that they represent a lossy storage
 * of transitions between segments of some larger composite.  Hence,
 * we need to define some sort of segmentation scheme that adherents
 * will use to express the relationship between their whole and their
 * parts.
 *
 * The n-gram will take care of all the rest of the storage specifics
 * such as window size, etc.
 */

trait SegmentLike[T, U] extends Ordering[U] {
  case class Extraction(part: U, remainder: T)

  val StartPart: U
  val EndPart: U
  def emptyPart: U
  def emptyWhole: T
  def combine(wholeSoFar: T, part: U): T
  def disassemble(whole: T): Iterator[U]
  def assemble(parts: Iterator[U]): T =
    parts.foldLeft(emptyWhole)((wholeSoFar, part) => combine(wholeSoFar, part))
  def decompose(whole: T): Iterator[U] =
    Seq(StartPart).iterator ++ disassemble(whole) ++ Seq(EndPart).iterator
  def compose(parts: Iterator[U]): T = {
    // validating the leading start token is simple
    if (parts.next != StartPart)
      throw new Exception("Invalid start segment")

    // extract the rest of the whole, along with how many end tokens you found,
    // and keep track of the very last token encountered (for validation)
    val (whole, numEnds, lastPart) = parts.foldLeft((emptyWhole, 0, emptyPart))((tSoFar, part) => tSoFar match {
      case (wholeSoFar, numEnds, lastPart) =>
        part match {
          case EndPart => (wholeSoFar, numEnds + 1, part)
          case _               => (combine(wholeSoFar, part), numEnds, part)
        }
    })
    if (numEnds != 1)
      throw new Exception("Did not find end segment")
    if (lastPart != EndPart)
      throw new Exception("Did not find end segment in the terminal position")

    // if you get here, you must have build up a valid whole
    whole
  }
  def partToString(part: U): String = part.toString
  def wholeToString(whole: T): String = whole.toString

  def compare(a: U, b: U): Int
}

