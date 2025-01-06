package aoc
package aoc2024.day10

import aoc2024.day10.TrailHead.{findTrailHeads, getTotalScore}
import common.Utils.loadData
import common.{Position, TwoDimensionalArray}
import scala.annotation.tailrec

type TopographicMap = Array[Array[Int]]
type Height = Int

object TrailHead {
  private val startingHeight = 0
  private val endingHeight = 9

  def findStartingPoints(map: TopographicMap): Set[Position] =
    (for {
      (row, rowIndex) <- map.zipWithIndex
      (column, columnIndex) <- row.zipWithIndex
      if map(rowIndex)(columnIndex) == startingHeight
    } yield Position(columnIndex, rowIndex)).toSet

  private def getAdjacentPositions(
      map: TopographicMap,
      position: Position
  ): Set[Position] =
    Set(
      Position(position.x - 1, position.y),
      Position(position.x + 1, position.y),
      Position(position.x, position.y - 1),
      Position(position.x, position.y + 1)
    ).filter(p =>
      p.x >= 0 && p.y >= 0 && p.x < map.head.length && p.y < map.length
    )

  def findTrailHeads(map: TopographicMap): Seq[Set[Position]] = {
    findStartingPoints(map).toSeq.map(position => {
      println(
        s"Finding Trailheads for Starting Point (${position.x} ${position.y})"
      )
      val result = findTrailHeadsRec(map, Set(position), startingHeight)
      println(s"Result: $result")
      result
    })
  }

  @tailrec
  private def findTrailHeadsRec(
      map: TopographicMap,
      accumulator: Set[Position],
      height: Height
  ): Set[Position] = {
    if (accumulator.isEmpty || height == endingHeight) {
      accumulator
    } else {
      val nextHeight = height + 1
      val validPositions = accumulator.flatMap(position =>
        TrailHead
          .getAdjacentPositions(map, position)
          .filter(p => map(p.y)(p.x) == nextHeight)
      )
      findTrailHeadsRec(map, validPositions, nextHeight)
    }
  }

  def getTotalScore(trailHeads: Seq[Set[Position]]): Int =
    trailHeads.map(_.size).sum

}

@main def main(): Unit = {

  val filename = "aoc2024-day10-input.txt"
//  val filename = "test.txt"
  val topographicMap: Array[Array[Int]] =
    loadData(filename)(TwoDimensionalArray.parseInput(_.asDigit))

  topographicMap.foreach(row => println(row.mkString("")))

  TrailHead
    .findStartingPoints(topographicMap)
    .foreach(point => println(point.toString))

  val trailHeads = findTrailHeads(topographicMap)
  println(s"Number of trailheads: ${trailHeads.size}")
  println(s"Trailheads: $trailHeads")
  println(s"Total score of trailheads: ${getTotalScore(trailHeads)}")

}
