package aoc
package aco2022.day6

import common.Utils.loadData

object MarkerDetector {

  def parseInput(
      lines: Iterable[String]
  ): List[List[Char]] = lines.toList.map(line => line.toList)

  def findFirstMarkers(signals: List[List[Char]], threshold: Int): List[Int] =
    signals.map(findFirstMarker(_, threshold))

  private def findFirstMarker(signal: List[Char], threshold: Int): Int =
    signal
      .sliding(threshold)
      .zipWithIndex
      .find(pair => pair._1.toSet.size == threshold)
      .get
      ._2 + threshold
}

@main def main(): Unit = {

  val filename = "aoc2022-day6-input.txt"
//  val filename = "test2.txt"
  val signals = loadData(filename)(MarkerDetector.parseInput)

  val markers = MarkerDetector.findFirstMarkers(signals, 4)
  println(s"Part 1 result: ${markers.mkString(" ")}")

  val markers2 = MarkerDetector.findFirstMarkers(signals, 14)
  println(s"Part 2 result: ${markers2.mkString(" ")}")

}
