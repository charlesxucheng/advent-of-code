package aoc2022
package day1

import scala.io.Source
import common.Utils._
@main def main(): Unit = {

  val filename = "day1-input.txt"
  val parser: Iterator[String] => List[List[Int]] = CalorieCalculator.parseInput
  println(s"The highest value is: ${CalorieCalculator.findMax(loadData[List[Int]](filename)(parser))}")
  println(s"The sum of the highest 3 values is: ${CalorieCalculator.findSumOfTopN(loadData[List[Int]](filename)(parser))}")
}
