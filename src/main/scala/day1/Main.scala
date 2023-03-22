package aoc2022
package day1

import scala.io.Source
@main def main(): Unit = {

  val filename = "day1-input.txt"
  println(s"The highest value is: ${CalorieCalculator.findMax(loadData(filename))}")
  println(s"The sum of the highest 3 values is: ${CalorieCalculator.findSumOfTopN(loadData(filename))}")
}
