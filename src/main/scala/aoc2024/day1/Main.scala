package aoc
package aoc2024.day1

import aoc2022.common.Utils.loadData

import scala.io.Source

object NumberPairs {
  def parseInput(lines: Iterator[String]): List[(Int, Int)] =
    lines
      .map(_.split(" {3}"))
      .map(pair => (pair(0).toInt, pair(1).toInt))
      .toList
}

@main def main(): Unit = {

  val filename = "aoc2024-day1-input.txt"
  val pairOfNumbers =loadData(filename)(NumberPairs.parseInput).unzip
  val list1 = pairOfNumbers._1.sorted
  val list2 = pairOfNumbers._2.sorted

  val totalDistance = list1.zip(list2).map(pair => Math.abs(pair._1 - pair._2)).sum
  println(s"The total distance is: $totalDistance")

  val similarityScore = list1.map(x => x * list2.count(_ == x)).sum
  println(s"The similarity score is: $similarityScore")
}

