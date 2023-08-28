package aoc2022
package day3

import ElfItem.ElfItem
import math.Integral.Implicits.infixIntegralOps
object DuplicateItemFinder {

  def findCommonItems(input: String): Set[ElfItem] = findCommonItems(ElfItem.from(input))

  def findCommonItems(items: List[ElfItem]): Set[ElfItem] = {
    require(items.length % 2 == 0)
    val mid = items.length / 2
    findCommonItems(items.take(mid), items.takeRight(mid))
  }

  private def findCommonItems(left: List[ElfItem], right: List[ElfItem]): Set[ElfItem] = {
    assert(left.length == right.length)
    left.toSet.intersect(right.toSet)
  }

  def parseInput(lines: Iterator[String]): List[List[ElfItem]] =
    lines.map(ElfItem.from).toList

  def sumPriority(items: Set[ElfItem]): Int = items.map(_.priority).sum

  def sumAllPriority(input: List[List[ElfItem]]): Int =
    input.map(findCommonItems).map(sumPriority).sum
}
