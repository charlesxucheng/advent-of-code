package aoc
package aco2022.day3

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

  def parseInput2(lines: Iterator[String]): List[(List[ElfItem], List[ElfItem], List[ElfItem])] = {
    val input = lines.toList
    require(input.size % 3 == 0)
    val result = input.grouped(3).toList.map(group => {
      (ElfItem.from(group.head), ElfItem.from(group(1)), ElfItem.from(group(2)))
    })
    result
  }

  def sumAllBadgePriority(input: List[(List[ElfItem], List[ElfItem], List[ElfItem])]): Int = {
    input.map(tuple => {
      val badges = tuple._1.toSet.intersect(tuple._2.toSet).intersect(tuple._3.toSet)
      badges.map(_.priority).sum
    }).sum
  }

}

