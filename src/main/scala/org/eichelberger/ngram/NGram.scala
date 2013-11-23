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
      val partsReverse: List[U] = window.reverse
      val ngWindow = partsReverse.tail.foldLeft(NGram[T,U](partsReverse.head, windowSize))((ngSoFar, part) =>
        NGram[T,U](part, windowSize) + ngSoFar
      )
      ngSoFar + ngWindow
    })
  }
}

import NGram._

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

  def sampleValue: Option[U] = randomChild.map(_.value)

  //@TODO confirm that this works when the n-gram has fewer than window-size plys
  private def nextSamplePart(partsSoFar: List[U]): List[U] = {
    // for identifying the parent, you only need the last few elements
    val lastParts: List[U] =
      if (windowSize == NoWindowSize) partsSoFar
      else partsSoFar.takeRight(windowSize - 1)

    lastParts.foldLeft(Option(this))((parentOptSoFar, part) => parentOptSoFar.flatMap(
      parentSoFar => {
        val result = if (parentSoFar.children.contains(part)) parentSoFar.children.get(part)
        else None
        result
      }
    )).getOrElse(throw new Exception("Could not find suitable parent for sampling"))

    val parent = lastParts.foldLeft(Option(this))((parentOptSoFar, part) => parentOptSoFar.flatMap(
      parentSoFar =>
        if (parentSoFar.children.contains(part)) parentSoFar.children.get(part)
        else None
    )).getOrElse(throw new Exception("Could not find suitable parent for sampling"))

    // generate a next value
    val nextValue = Option(parent.sampleValue.getOrElse(parent.value))
    if (nextValue.isEmpty) throw new Exception("Could not generate sample value from parent")

    // recurse, if you haven't hit an end-of-sequence token
    val newParts: List[U] = partsSoFar ++ nextValue
    if (nextValue.get == ev.EndPart) newParts
    else nextSamplePart(newParts)
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
}