package org.eichelberger.ngram

/**
 * An n-gram -- see http://en.wikipedia.org/wiki/N-gram -- is a tree structure that
 * summarizes sequences by counting transitions between elements, with the maximum
 * sub-sequence length -- the "n" in "n-gram" being set in advance.
 *
 * For example, words are sequences of letters (characters), so if we built an
 * n-gram from a large sample of English words, we would expect that the letter
 * "Q" would be, almost always, followed by the letter "U" (assuming a relatively
 * low incidence of Cat words as in http://www.youtube.com/watch?v=v2u_DVkK1Rc).
 *
 * To encourage coherence, sequences are assumed to begin with a [START] token,
 * and end with an [END] token, so storing the string "foo" would actually
 * represent 5 tokens:  "[START]", "f", "o", "o", "[END]"; the window-size
 * would be applied as many times as possible, so a window size of 3 would
 * yield these sub-strings, each inserted into the n-gram tree:
 * - [START]-f-o
 * - f-o-o
 * - o-o-[END]
 *
 * Example uses of an n-gram:
 * - Generate the most likely next token from a sub-sequence:  Say you had
 *   aggregated search-strings within an n-gram.  Given a partial search-
 *   string of "n-gra", you could make a prediction (suggestion) that the
 *   next letter will be "m", based on the history of earlier entries.
 * - Generate probabilistic samples:  If you wanted to name a new drug, one
 *   (simplistic) approach would be to build an n-gram of all of the other
 *   drug brand names, and then generate a list of sequences from the n-gram
 *   (filtering out those already corresponding with real names).
 * - Estimate probability of a sequence:  Say you stored the ZIP codes of
 *   all of your customers in an n-gram, then you could ask what fraction
 *   of your customers had a ZIP code that began with "229" (giving you
 *   a very rough, and probably non-contiguous geographic range).  In fact,
 *   you could generate an (admittedly odd-looking) chloropleth map from
 *   such an n-gram (and make it hierarchical, supporting drill-through).
 *
 * Note that n-grams are additive, meaning that they allow for distributed
 * aggregation:  If your data were partitioned into 10 separate units, each
 * unit could be summarized in an n-gram, and then the n-grams could be
 * combined directly without losing any (more) information, giving you a
 * summary of the entire data set.  This might be interesting for NoSQL
 * applications similar to the VACUUM/ANALYZE within PostgreSQL.
 *
 * The fidelity of the n-gram is related to its depth:  The shallower the n-gram,
 * the less faithful will be its aggregation, but the less storage space will be
 * required.  Depth in this n-gram is referred to as "windowSize".
 *
 * Another CRUCIAL impact of the n-gram depth is cycling:  Consider building an
 * n-gram of words from this sentence:  "i am what i am"
 * (punctuation and capitalization elided intentionally).  If the depth is 2,
 * the windowed sub-sequences that will be presented to the n-gram are:
 *
 *   [START]-i
 *   i-am
 *   am-what
 *   what-i
 *   i-am
 *   am-[END]
 *
 * yielding this n-gram:
 *
 *   [ROOT]
 *   |
 *   +-- [START]
 *   |   |
 *   |   +-- i
 *   |       |
 *   |       +-- am
 *   |
 *   +-- i (2)
 *   |   |
 *   |   +-- am (2)
 *   |
 *   +-- am (2)
 *   |   |
 *   |   +-- what
 *   |   |
 *   |   +-- [END]
 *   |
 *   +-- what
 *       |
 *       +-- i
 *
 * There is a possibility that drawing a sample from this n-gram will result in
 * a sequence that does not terminate:  "[START]-i-am-what-i-am-what-i-am-..."
 * The possible presence of this kind of cycle complicates sampling.
 *
 * The estimate of sequence probabilities is degraded by the number of sub-
 * sequences shared among different presentations.  Consider storing two strings
 * "Abbbbc" and "Dbbbbe" with a window size of 3; the shared cycle will lower
 * the estimated probability of either real (observed) presentation.
 *
 * The reader deserves a more cogent description of n-grams, their uses, and
 * limitations.  Perhaps some kind soul will someday provide one here...
 * or at least reference a better discussion elsewhere...
 *
 * To implement n-grams, we have chosen a two-part approach:  This file represents
 * the destination for data, the n-gram itself; the other part is the SegmentLike
 * trait that defines the type of sequence data that can be stored in an n-gram.
 */

// an n-gram must be built on top of a type that is segmented

object NGram {
  val NoWindowSize = 0

  def apply[T, U](windowSize: Int = NoWindowSize)(implicit ev: SegmentLike[T, U]): NGram[T, U] =
    new NGram[T, U](ev.emptyPart, 0L, Map.empty[U, NGram[T,U]], windowSize, 0)(ev)

  def apply[T, U](value: U, windowSize: Int = NoWindowSize)(implicit ev: SegmentLike[T, U]): NGram[T, U] =
    new NGram[T, U](value, 1L, Map.empty[U, NGram[T,U]], windowSize, 1)(ev)

  def apply[T, U](value: U, windowSize: Int, maxDepth: Int)(implicit ev: SegmentLike[T, U]): NGram[T, U] =
    new NGram[T, U](value, 1L, Map.empty[U, NGram[T,U]], windowSize, maxDepth)(ev)

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
      val ngWindow = partsReverse.tail.foldLeft(NGram[T,U](partsReverse.head, windowSize, parts.size))((ngSoFar, part) =>
        NGram[T,U](part, windowSize) + ngSoFar
      )
      ngSoFar + ngWindow
    })
  }
}

import NGram._

/**
 * The n-gram is a recursive data structure, representing both a tree and a node.
 *
 * @param value the single segment associated with this node
 * @param count how many instances
 * @param children the association of next tokens with child nodes
 * @param windowSize the maximum depth of any sub-sequence within the tree
 * @param maxDepthPresented the maximum length of any single sequence anywhere in the tree
 * @param ev the sequence-like evidence that knows how to handle the sequence
 * @tparam T the sequence type (such as "word")
 * @tparam U the sequence-element type (such as "letter within a word")
 */

case class NGram[T, U](value: U, count: Long, children: Map[U, NGram[T,U]], windowSize: Int, maxDepthPresented: Int)(implicit ev: SegmentLike[T, U]) {
  // a window-size of 1 is degenerate
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

  // this is intended for display, not for serialization
  override def toString: String =
    s"'${ev.partToString(value)}':$windowSize:$count=${children.map(kv => kv._2.toString).mkString("<", ", ", ">")}"

  // yields a tree-like structure suitable for console display
  def prettyPrint(level: Int = 0, ongoing: Map[Int,Boolean] = Map.empty[Int,Boolean]) {
    val leader = (0 until level).map(i => if (ongoing.contains(i)) "|  " else "   ").mkString
    println(s"$leader+- '${ev.partToString(value)}' ($count)")

    (0 until children.size).map(i => {
      val child: NGram[T, U] = children.values.drop(i).take(1).head
      val nextOngoing = if (i < (children.size - 1)) ongoing + ((level + 1) -> true) else ongoing
      child.prettyPrint(level+1, nextOngoing)
    })
  }

  // yields the result of adding a new sequence to this n-gram
  def +(whole: T): NGram[T, U] = this + NGram.fromWhole[T, U](whole, this.windowSize)

  // yields the result of adding another n-gram to this one
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
      this.copy(
        count = newCount, children = newChildren,
        maxDepthPresented = Math.max(maxDepthPresented, that.maxDepthPresented))
    } else {
      // this is a non-peer node not matching any children; just add it
      val newChildren = children + (that.value -> that)
      val newCount = newChildren.mapValues(_.count).map(_._2).sum
      this.copy(
        count = newCount, children = newChildren,
        maxDepthPresented = Math.max(maxDepthPresented, that.maxDepthPresented))
    }
  }

  // probabilistically selects a child based on counts via roulette-wheel selection
  private def randomChild: Option[NGram[T, U]] =
    if (children.size > 0) {
      // roulette-wheel selection
      val x = math.random * childSum
      cumulativeChildCounts.find(_.cumulativeSum >= x)
        .map(cs => children(cs.value))
    } else None

  /**
   * There are applications, such as index planning, in which it can be useful to
   * know how concentrated the n-gram aggregate is.  In some cases, it is useful
   * to think of the highest expected probability for a single sequence (sample),
   * which is what this function estimates.
   *
   * It will only be an estimate, because there is no guarantee that the n-gram's
   * aggregate is deterministic (because of recurrent sub-sections within the
   * presentation sequences).
   *
   * Furthermore, a greedy approach is inappropriate, because the frequencies are
   * driven by the transition among elements.  Imagine the following n-gram:
   *
   *   [ROOT]
   *   |
   *   +-- [START]
   *   |   |
   *   |   +-- A (5)
   *   |   |   |
   *   |   |   +-- B (5)
   *   |   |
   *   |   +-- B (7)
   *   |       |
   *   |       +-- C (4)
   *   |       |
   *   |       +-- D (3)
   *   |
   *   +-- [ so on and so forth... ]
   *
   * In the preceding example, the greedy approach would claim that "[START]-B" is
   * the most-frequent choice, where clearly "[START]-A" is better, because A's
   * single child concentrates the frequency better (not knowing more about the
   * subsequent, deeper parts of the n-gram tree).  (NB:  If the maximum sample
   * depth were set to 2, then "[START]-B" would, in fact, be the highest-
   * frequency answer.)
   *
   * To accommodate this vagary, we conduct a more thorough exploration of the
   * n-gram tree for the highest-frequency sequences, pruning out choices as
   * quickly as possible.
   *
   * @param maxDepth how many levels deep you are allowed to search; defaults to the
   *                 maximum length of any presentation seen so far
   * @return a sample of fixed length for which no other sample has a higher
   *         expected frequency-of-occurrence
   */

  def getMostFrequent(maxDepth: Int = maxDepthPresented): (List[U], Double) = {
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

  def sampleValue: Option[U] = randomChild.map(_.value)

  // It is often necessary to find the penultimate node in a sequence.
  // This routine is responsible for doing that.

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

  // utility method to ensure that we haven't done anything horrible in the
  // construction of our tree to violate key assumptions
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

  // assumes that every presentation begins with a START token
  // defined by the evidence parameter
  def presentationCount: Long = countValues(ev.StartPart)
}