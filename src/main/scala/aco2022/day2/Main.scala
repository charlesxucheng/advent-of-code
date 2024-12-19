package aoc
package aco2022.day2

import common.Utils.loadData

import scala.io.Source

@main def main(): Unit = {

  val filename = "day2-input.txt"
  val data = loadData(filename)(PaperScissorsStoneScoreCalculator.parseInput)
  println(s"Part1: the total score is: ${PaperScissorsStoneScoreCalculator.calculateTotalScore(data)}")

  val dataPart2 = loadData(filename)(PaperScissorsStoneScoreCalculator.parseInputPart2)
  println(s"Part 2: the total score is: ${PaperScissorsStoneScoreCalculator.calculateTotalScorePart2(dataPart2)}")
}
