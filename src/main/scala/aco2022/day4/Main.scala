package aoc
package aco2022.day4

import aco2022.day3.DuplicateItemFinder

import common.Utils.loadData

@main def main(): Unit = {
  val filename = "day4-input.txt"
  val part1Data = loadData(filename)(DuplicateItemFinder.parseInput)
  println(s"Part1: the total priority is: ${DuplicateItemFinder.sumAllPriority(part1Data)}")
  val part2Data = loadData(filename)(DuplicateItemFinder.parseInput2)
  println(s"Part2: the total priority is: ${DuplicateItemFinder.sumAllBadgePriority(part2Data)}")
}