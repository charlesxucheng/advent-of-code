package aoc2022
package day2

import scala.io.Source

@main def main(): Unit = {

  val filename = "day2-input.txt"
  println(s"The total score is: ${PaperScissorsStoneScoreCalculator.calculateTotalScore(loadData(filename))}")
}
