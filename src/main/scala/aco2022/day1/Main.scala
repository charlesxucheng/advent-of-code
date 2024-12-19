package aoc
package aco2022.day1

import scala.io.Source
import common.Utils._
@main def main(): Unit = {

  val filename = "day1-input.txt"
  println(s"The highest value is: ${CalorieCalculator.findMax(loadData(filename)(CalorieCalculator.parseInput))}")
  println(s"The sum of the highest 3 values is: ${CalorieCalculator.findSumOfTopN(loadData(filename)(CalorieCalculator.parseInput))}")
}
