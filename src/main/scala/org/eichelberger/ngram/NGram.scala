package org.eichelberger.ngram

// an n-gram must be built on top of a type that is segmented

object NGram {
  val NoWindowSize = 0

  def apply[T, U](windowSize: Int = NoWindowSize)(implicit ev: SegmentLike[T, U]): NGram[T, U] =
    new NGram[T, U](ev.emptyPart, 0L, Map.empty[U, NGram[T,U]], windowSize)(ev)

  def apply[T, U](value: U, windowSize: Int = NoWindowSize)(implicit ev: SegmentLike[T, U]): NGram[T, U] =
    new NGram[T, U](value, 1L, Map.empty[U, NGram[T,U]], windowSize)(ev)

  def fromWhole[T, U](whole: T, windowSize: Int = NoWindowSize)(implicit ev: SegmentLike[T, U]): NGram[T, U] = {
    val parts: List[U] = ev.decompose(whole).toList
    val partsWindows: List[List[U]] = windowSize match {
      case NoWindowSize => List(parts)  // no limit; use all parts as one sequence
      case _            => parts.sliding(windowSize).toList
    }

    partsWindows.foldLeft(NGram[T,U](windowSize))((ngSoFar, window) => {
      if (window.size < windowSize && window.last != ev.EndPart)
        throw new Exception(s"Short sequence does not end with EndPart:  ${window.map(_.toString).mkString("<", ">, <", ">")}")

      val partsReverse: List[U] = window.reverse
      val ngWindow = partsReverse.tail.foldLeft(NGram[T,U](partsReverse.head, windowSize))((ngSoFar, part) =>
        NGram[T,U](part, windowSize) + ngSoFar
      )
      ngSoFar + ngWindow
    })
  }
}

import NGram._
import scala.util.Try
import scala.util.Success
import scala.util.Failure

case class NGram[T, U](value: U, count: Long, children: Map[U, NGram[T,U]], windowSize: Int)(implicit ev: SegmentLike[T, U]) {
  require(windowSize == NoWindowSize || windowSize > 1)

  lazy val childSum: Long = (children.map { _ match { case (k, v) => v.count }}).sum

  case class CumulativeChildCount(value: U, cumulativeSum: Long)

  lazy val cumulativeChildCounts: List[CumulativeChildCount] =
  // @TODO make this a bit friendlier?
    children.foldLeft((0L, List[CumulativeChildCount]()))((t, childKV) => {
      val cumulativeSum = t._1 + childKV._2.count
      val newChild = CumulativeChildCount(childKV._1, cumulativeSum)
      (cumulativeSum, t._2 ::: List(newChild))
    })._2

  lazy val nodeCount: Int = 1 + children.map { case (k,v) => v.nodeCount }.toList.sum

  override def toString: String =
    s"'${ev.partToString(value)}':$windowSize:$count=${children.map(kv => kv._2.toString).mkString("<", ", ", ">")}"

  def prettyPrint(level: Int = 0, ongoing: Map[Int,Boolean] = Map.empty[Int,Boolean]) {
    val leader = (0 until level).map(i => if (ongoing.contains(i)) "|  " else "   ").mkString
    println(s"$leader+- '${ev.partToString(value)}' ($count)")

    (0 until children.size).map(i => {
      val child: NGram[T, U] = children.values.drop(i).take(1).head
      val nextOngoing = if (i < (children.size - 1)) ongoing + ((level + 1) -> true) else ongoing
      child.prettyPrint(level+1, nextOngoing)
    })
  }

  def +(whole: T): NGram[T, U] = this + NGram.fromWhole[T, U](whole, this.windowSize)

  def +(that: NGram[T, U]): NGram[T, U] =
    if (that.value == ev.emptyPart) {
      that.children.foldLeft(this)((ngSoFar, childKV) => ngSoFar.blend(childKV._2))
    } else blend(that)

  // assume that you are accumulating what is meant to be a new child
  private def blend(that: NGram[T, U]): NGram[T, U] = {
    // do you already have a child to blend with this non-peer?
    if (children.contains(that.value)) {
      // beneath this shared node, combine all children
      val newChild = if (that.children.size < 1) {
        val oldCount = children(that.value).count
        children(that.value).copy(count = oldCount + that.count)
      }
      else that.children.foldLeft(children(that.value))((childNGSoFar, childKV) => childNGSoFar.blend(childKV._2))
      val newChildren = children + (that.value -> newChild)
      val newCount = newChildren.mapValues(_.count).map(_._2).sum
      this.copy(count = newCount, children = newChildren)
    } else {
      // this is a non-peer node not matching any children; just add it
      val newChildren = children + (that.value -> that)
      val newCount = newChildren.mapValues(_.count).map(_._2).sum
      this.copy(count = newCount, children = newChildren)
    }
  }

  private def randomChild: Option[NGram[T, U]] =
    if (children.size > 0) {
      // roulette-wheel selection
      val x = math.random * childSum
      cumulativeChildCounts.find(_.cumulativeSum >= x)
        .map(cs => children(cs.value))
    } else None

  /*

   APPROACH:

   Keep a list of ALL partials and their frequencies, but
   1.  keep it sorted
   2.  each time you add a new item, delete all records that both
       A.  have a shorter length; AND
       B.  have a lower frequency

   */

//  def getMostFrequent(maxDepth: Int = 3): (List[U], Double) = {
//    case class PQEntry(partial: List[U], frequency: Double)
//
//    def updatePQ(pq: List[PQEntry], entry: PQEntry): List[PQEntry] = {
//      // there must not exist any full-size entry whose frequency is at least as good
//      def isAcceptable =
//        !pq.exists(e => e.partial.size == maxDepth && e.frequency >= entry.frequency) &&
//        !(entry.frequency == 0.0 && pq.exists(e => e.partial.size >= entry.partial.size && e.frequency == 0.0))
//
//      // if this is a full-size entry, but there are better full-size entries already present, don't add it
//      if (isAcceptable) {
//        // if this entry is full-size, remove other (known worse) full-size entries
//        val r1 =
//          if (entry.partial.size == maxDepth) pq.filter(_.partial.size < maxDepth)
//          else pq
//
//        // if there is more than one entry whose frequency is zero, keep only the longest (known to be this)
//        val r2 =
//          if (entry.frequency == 0.0) r1.filter(_.frequency > 0.0)
//          else r1
//
//        //@TODO replace
//        entry :: r2
//      } else pq  // unchanged, because the candidate was not acceptable
//    }
//
////    def getCandidate(pq: List[PQEntry]): Option[PQEntry] = {
////      val unterminated = pq.filter(_.partial.last != ev.EndPart)
////      val minLength = unterminated.map(_.partial.size).min
////      if (minLength == maxDepth) None
////      else unterminated.find(_.partial.size == minLength)
////    }
////
////    def extend(pq: List[PQEntry]): List[PQEntry] = {
////      getCandidate(pq) match {
////        case Some(nextCandidate) =>
////          // find the parent node associated with this sequence of partials
////          val parent = getLastNode(nextCandidate.partial)
////
////          // add all of this candidate's children to the priority queue
////          val entries = parent.children.toList.map(child => new PQEntry(
////            nextCandidate.partial ++ List(child._1),
////            nextCandidate.frequency * parent.count.toDouble / child._2.count.toDouble
////          ))
////          val nextPQ: List[PQEntry] = (entries /: pq) {
////            case (pqSoFar, entry) => updatePQ(pqSoFar, entry)
////          }
////
////          extend(nextPQ)
////        case _ => pq  // do nothing; you're done
////      }
////    }
//
//    def recurse(pq: List[PQEntry], parts: List[U]): List[PQEntry] = {
//      if (parts.last != ev.EndPart && parts.size < maxDepth) {
//        val parent = getLastNode(parts);
//        (parent.children.toList /: pq) {
//          case (pqSoFar, childKV) =>
//            val a = pqSoFar
//            val b = childKV
//            val nextParts = parts ++ List(childKV._1)
//            val entry = PQEntry(
//              nextParts,
//              estimateProbability(nextParts)
//            )
//            updatePQ(pqSoFar, entry)
//        }
//      } else pq
//    }
//
//    //val pq: List[PQEntry] = extend(List[PQEntry](new PQEntry(List(ev.StartPart), 1.0)))
//    val pq: List[PQEntry] = recurse(List[PQEntry](), List(ev.StartPart))
//    val best = pq.head
//    (best.partial, best.frequency)
//  }

  def getMostFrequent(maxDepth: Int = 3): (List[U], Double) = {
    case class Entry(partial: List[U], frequency: Double) {
      def extend(part: U): Entry = {
        val newPartial = partial ++ List(part)
        val newFrequency = estimateProbability(newPartial)
        Entry(newPartial, newFrequency)
      }
    }

    def recurse(current: Entry, bestSoFar: Option[Entry]): Option[Entry] = {
      if (current.partial.size <= maxDepth) {
        // you can still recurse
        if (current.partial.last != ev.EndPart) {
          // you have not yet terminated, and there are levels of recursion left to use
          if (bestSoFar.isEmpty || current.frequency > bestSoFar.get.frequency) {
            // there's still room for improvement down this chain
            val parent = getLastNode(current.partial)
            parent.children.toList.foldLeft(bestSoFar)((soFar, child) => {
              val childParts = current.partial ++ List(child._1)
              val childFreq = current.frequency * child._2.count.toDouble / parent.count.toDouble
              recurse(Entry(childParts, childFreq), soFar)
            })
          } else bestSoFar
        } else bestSoFar  // you've hit an end-of-sequence token early
      } else {
        // already bottomed out; return the best encountered so far
        bestSoFar.map(soFar => if (soFar.frequency > current.frequency) soFar else current).orElse(Option(current))
      }
    }

    recurse(Entry(List(ev.StartPart), 1.0), None) match {
      case Some(entry) => (entry.partial, entry.frequency)
      case None        => (Nil, 0.0)
    }
  }


  def old_getMostFrequent(maxDepth: Int = 3): (List[U], Double) = {
    def getMostFrequentParts(partsSoFar: List[U], parentCount: Long, depthRemaining: Int): (List[U], Double) = {
      // identify the parent node with maximal context
      val nodeTry = Try { getLastNode(partsSoFar.takeRight(windowSize - 1)) }

      // if you couldn't find a parent node, be sure you have a good reason
      // (such as:  the sequence was already ended)
      val node: NGram[T, U] = nodeTry match {
        case Success(someNode) => someNode
        case Failure(_) =>
          if (partsSoFar.last == ev.EndPart) null
          else throw new Exception("Could not find parent for un-terminated parts list:  " +
            partsSoFar.map(_.toString).mkString(", "))
      }

      val p = node match {
        case null => 1.0 / parentCount.toDouble  // @TODO refine this speculative return-value!
        case _ => node.count.toDouble / parentCount.toDouble
      }

      // the action to take depends upon how much depth remains
      depthRemaining match {
        case n if n <= 0 =>
          // you've bottomed out
          (partsSoFar, p)
        case _ if node == null || node.children.size < 1 =>
          // you've not bottomed out, but you have no children
          (partsSoFar, p)
        case _ =>
          // there's some depth left, and you have at least one child
          val recursives: Seq[(List[U], Double)] = node.children.toList.map(child =>
            getMostFrequentParts(partsSoFar ++ List(child._1), node.count, depthRemaining - 1))
          // use only the highest-frequency expectation among all of your child values
          val (parts, frequency) = recursives.sortWith((a, b) => a._2 < b._2).last
          (parts, p * frequency)
      }
    }

    // you must start with a beginning-of-sequence token (which does not accumulate against the count)
    val (parts, frequency) = getMostFrequentParts(List(ev.StartPart), count, maxDepth)

    // simple validation
    if (parts.head != ev.StartPart)
      throw new Exception(s"Invalid most-frequent sequence; does not begin with start-token:  ${parts.map(_.toString).mkString(", ")}")

    //@TODO cne1x debug
    println(s"[MOST FREQUENT] parts ($maxDepth) = ${parts.map(_.toString).mkString("{", ", ", "}")}; frequency $frequency")

    (parts, frequency)
  }

  def sampleValue: Option[U] = randomChild.map(_.value)

  // special case:
  // if the parts-so-far already end in an end-of-sequence marker,
  // and there are fewer than window-size parts in the list,
  // then you're not guaranteed to find any good matching node (because
  // there may have been no such windowed sequence ever added)
  def getLastNode(partsSoFar: List[U]): NGram[T, U] = {
    val lastParts: List[U] =
      if (windowSize == NoWindowSize) partsSoFar
      else partsSoFar.takeRight(windowSize - 1)

    lastParts.foldLeft(Option(this))((parentOptSoFar, part) => parentOptSoFar.flatMap(
      parentSoFar =>
        if (parentSoFar.children.contains(part)) parentSoFar.children.get(part)
        else None
    )).getOrElse({
      lastParts.foldLeft(Option(this))((parentOptSoFar, part) => parentOptSoFar.flatMap(
        parentSoFar =>
          if (parentSoFar.children.contains(part)) parentSoFar.children.get(part)
          else None
      ))
      throw new Exception("Could not find suitable parent for sampling")
    })
  }

  //@TODO confirm that this works when the n-gram has fewer than window-size plys
  private def nextSamplePart(partsSoFar: List[U]): List[U] = {
    try {
      // identify the bottom-most parent
      val parent = getLastNode(partsSoFar)

      if (parent.children.size < 1) {
        throw new Exception("Parent unexpectedly contains no children:  ")
        parent.prettyPrint()
      }

      // generate a next value
      val nextValue = Option(parent.sampleValue.getOrElse(parent.value))
      if (nextValue.isEmpty) throw new Exception("Could not generate sample value from parent")

      // recurse, if you haven't hit an end-of-sequence token
      val newParts: List[U] = partsSoFar ++ nextValue
      if (nextValue.get == ev.EndPart) newParts
      else nextSamplePart(newParts)
    } catch {
      // you were unable to find a parent-match for the sequence so far;
      // this MAY not be a problem, if the partial sequence already has an end-of-sequence marker
      case ex: Exception =>
        if (partsSoFar.last == ev.EndPart) partsSoFar
        else throw new Exception("Could not find suitable parent for terminal node of partial sequence:  " +
          partsSoFar.map(_.toString).mkString(", "))
    }
  }

  // generate one plausible whole from parts that are chosen via
  // roulette-wheel selection
  def sample: T = ev.compose(nextSamplePart(List[U](ev.StartPart)).iterator)

  // leave off the end-of-sequence token; we want the probability that any
  // data might begin (and not necessarily end) with this prefix
  def estimatePrefixProbability(whole: T): Double =
    estimateProbability(ev.decompose(whole).toList.dropRight(1))

  def estimateProbability(whole: T): Double = estimateProbability(ev.decompose(whole).toList)

  private def estimateProbability(parts: List[U]): Double = {
    // list all of the sequences whose terminal-node probability matters
    val partialTerminals: List[List[U]] =
    // skip the initial START marker probability, and treat it as 1.0
      (2 until math.min(parts.size + 1, windowSize)).map(upperExclusive =>
        parts.slice(0,upperExclusive)).toList
    val windowedTerminals: List[List[U]] = if (parts.size >= windowSize) {
      parts.sliding(windowSize).toList
    } else Nil
    val terminals = partialTerminals ::: windowedTerminals

    def getTerminalProbability(terminalList: List[U]): Double = {
      val (parentCount: Long, child: Option[NGram[T,U]]) = terminalList.foldLeft((0L, Option(this)))((t, value) => t match { case (totalSoFar, ngSoFar) =>
        if (ngSoFar.isDefined && ngSoFar.get.children.contains(value)) (ngSoFar.get.childSum, ngSoFar.get.children.get(value))
        else (0L, None)
      })

      if (parentCount > 0L && child.isDefined) child.get.count.toDouble / parentCount.toDouble
      else 0.0
    }

    terminals.map(terminal => getTerminalProbability(terminal)).product
  }

  def countValues(target: U): Long = {
    val initial: Long = if (value == target) count else 0L

    children.foldLeft(initial)((soFar, child) => soFar + child._2.countValues(target))
  }

  def validate {
    if (value != ev.emptyPart)
      throw new Exception(s"Root node should be empty; instead was:  <$value>}")

    val numStart = countValues(ev.StartPart)
    val numEnd = countValues(ev.EndPart)
    if (numStart != numEnd)
      throw new Exception(s"Mis-matching start, end counts:  $numStart != $numEnd")

    def ensureLength(node: NGram[T, U], remainingDepth: Int) {
      if (remainingDepth < 0)
        throw new Exception(s"Underflow in remainingDepth $remainingDepth")
      if (remainingDepth == 0 && node.children.size > 0)
        throw new Exception(s"Maximum depth exceeded; children = ${node.children.map(_._1.toString).mkString("|")}")
      if (node.children.size < 1 && remainingDepth > 0 && node.value != ev.EndPart)
        throw new Exception(s"Premature termination:  Ends in non-terminal <$node.value> with $remainingDepth layers still expected")
      if (node.children.size > 0)
        node.children.map(child => ensureLength(child._2, remainingDepth - 1))
    }
    children.map(child => ensureLength(child._2, windowSize - 1))
  }
}