package aoc
package aco2022.day4

import scala.annotation.tailrec

object SectionRange {
  def contains(r1: SectionRange, r2: SectionRange): Boolean =
    r1.contains(r2) || r2.contains(r1)

  @tailrec
  private def disjoint(r1: SectionRange, r2: SectionRange): Boolean =
    if (r1.start <= r2.start) r1.end < r2.start
    else disjoint(r2, r1)
}
case class SectionRange(start: Int, end: Int) {
  def contains(other: SectionRange): Boolean =
    this.start <= other.start && this.end >= other.end

  def overlapsWith(other: SectionRange): Boolean =
    !SectionRange.disjoint(this, other)
}

object OverlapSectionFinder {
  def parseInput(
      lines: Iterable[String]
  ): List[(SectionRange, SectionRange)] = {
    val pattern = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r
    lines.map { case pattern(start1, end1, start2, end2) =>
      val r1 = SectionRange(start1.toInt, end1.toInt)
      val r2 = SectionRange(start2.toInt, end2.toInt)
      (r1, r2)
    }.toList
  }
}
