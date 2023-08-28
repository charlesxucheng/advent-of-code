package aoc2022
package day3

import common.Utils.loadData

import aoc2022.day2.PaperScissorsStoneScoreCalculator

@main def main(): Unit = {
  val filename = "day3-input.txt"
  val data = loadData(filename)(DuplicateItemFinder.parseInput)
  println(s"Part1: the total score is: ${DuplicateItemFinder.sumAllPriority(data)}")
}