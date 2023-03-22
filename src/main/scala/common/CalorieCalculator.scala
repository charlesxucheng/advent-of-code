package aoc2022
package common

import scala.io.Source

def loadData(filename: String): List[List[Int]] = {
  val source = Source.fromFile(filename)
  try {
    CalorieCalculator.parseInput(source.getLines())
  } finally source.close()
}

object CalorieCalculator {

  def parseInput(input: String): List[List[Int]] = {
    if (input == null || input.isEmpty)
      List.empty
    else
      input.split(";;").toList
        .map(_.split(";"))
        .map(_.map(_.toInt).toList)
  }
  def parseInput(input: Iterator[String]): List[List[Int]] = {
    if (input == null || input.isEmpty)
      List.empty
    else
      val inputStr = input.map(_.trim).mkString(";")
      parseInput(inputStr)
  }

  def findMax(input: List[List[Int]]): Int = input.map(_.sum) match {
    case Nil => throw new IllegalArgumentException("Empty input. Cannot find max.")
    case x => x.max
  }
}

class CalorieCalculator() {
}