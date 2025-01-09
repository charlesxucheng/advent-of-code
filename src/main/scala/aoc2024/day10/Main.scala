package aoc
package aoc2024.day10

import aoc2024.day10.TrailHead.{findTrailHeads, getTotalRating, getTotalScore}
import common.Utils.loadData
import common.{Position, TwoDMap, TwoDimensionalArray}
import scala.annotation.tailrec

type TopographicMap = TwoDMap[Int]
type Height = Int
type Path = Seq[Position]

object TrailHead {
  private val startingHeight = 0
  private val endingHeight = 9

  def findStartingPoints(map: TopographicMap): Set[Position] =
    map.findAll(startingHeight)

  def findTrailHeads(map: TopographicMap): Set[Set[Path]] = {
    findStartingPoints(map).map(startingPosition => {
      println(
        s"Finding Trailheads for Starting Point $startingPosition"
      )
      val result =
        findTrailHeadsRec2(map, Set(Seq(startingPosition)), startingHeight)
      println(s"Result Set Size: ${getTrailHeadScore(result)}")
      result
    })
  }

  @tailrec
  private def findTrailHeadsRec2(
      map: TopographicMap,
      accumulator: Set[Path],
      height: Height
  ): Set[Path] = {
    if (accumulator.isEmpty || height == endingHeight) {
      accumulator
    } else {
      val nextHeight = height + 1
      val newPaths = accumulator.map(path =>
        map
          .getAdjacentPositions(path.last)
          .filter(p => map.get(p) == nextHeight)
          .map(position => path.appended(position))
      )

      findTrailHeadsRec2(map, newPaths.flatten, nextHeight)
    }
  }

  private def getTrailHeadScore(paths: Set[Path]): Int = paths.map(_.last).size

  def getTotalScore(pathsByStartingPoint: Set[Set[Path]]): Int =
    pathsByStartingPoint.toSeq.map(getTrailHeadScore).sum

  private def getTrailHeadRating(paths: Set[Path]): Int = paths.size

  def getTotalRating(pathsByStartingPoint: Set[Set[Path]]): Long =
    pathsByStartingPoint.toSeq.map(getTrailHeadRating).sum
}

@main def main(): Unit = {

  val filename = "aoc2024-day10-input.txt"
//  val filename = "test.txt"
  val topographicMap: TwoDMap[Int] =
    loadData(filename)(TwoDMap.parseInput(_.asDigit))

  topographicMap.display()

  TrailHead
    .findStartingPoints(topographicMap)
    .foreach(point => println(point.toString))

  val allPaths = findTrailHeads(topographicMap)

  println(s"Number of trailheads: ${allPaths.size}")
  println(s"Total score of trailheads: ${getTotalScore(allPaths)}")
  println(s"Total rating of trailheads: ${getTotalRating(allPaths)}")

}
