package aoc
package aco2022.day4

object SectionRange {
  def contains(r1: SectionRange, r2: SectionRange): Boolean = r1.contains(r2) || r2.contains(r1)
}
case class SectionRange(start: Int, end: Int) {
  def contains(other: SectionRange): Boolean = this.start <= other.start && this.end >= other.end
}


object OverlapSectionFinder {
  def findOverlap(lines: List[String]): Int = ???

}
