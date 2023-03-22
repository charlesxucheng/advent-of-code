package aoc2022
package day1

import common.{CalorieCalculator, loadData}

import scala.io.Source
@main def main(): Unit = {

  val filename = "day1-input.txt"
  println(s"Answer is: ${CalorieCalculator.findMax(loadData(filename))}")
}
