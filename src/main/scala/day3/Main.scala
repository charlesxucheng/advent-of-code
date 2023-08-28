package aoc2022
package day3

import common.Utils.loadData

import aoc2022.day2.PaperScissorsStoneScoreCalculator

@main def main(): Unit = {
  val filename = "day3-input.txt"
  val part1Data = loadData(filename)(DuplicateItemFinder.parseInput)
  println(s"Part1: the total priority is: ${DuplicateItemFinder.sumAllPriority(part1Data)}")
  val part2Data = loadData(filename)(DuplicateItemFinder.parseInput2)
  println(s"Part2: the total priority is: ${DuplicateItemFinder.sumAllBadgePriority(part2Data)}")
}