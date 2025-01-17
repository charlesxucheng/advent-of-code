package aoc
package aoc2024.day16

import common.TwoDMap
import common.Utils.loadData

implicit val ordering: Ordering[QueueElement] =
  Ordering.by[QueueElement, Long](_.move.cost).reverse

@main def main(): Unit = {
  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Info))
    .replace()

  val filename = "aoc2024-day16-input.txt"
//  val filename = "test.txt"

  val map = loadData(filename)(TwoDMap.parseInput(identity))
  map.display()

  val maze = ReindeerMaze.createMap(map)
  val costAndPositions = maze.shortestPathCostAndPositions()
  scribe.info(s"Shortest path cost: ${costAndPositions._1}")
  scribe.debug(
    s"Positions covered by all shortest paths: ${costAndPositions._2.toSeq
        .sortWith((x, y) => x._1 < y._1)}"
  )
  scribe.info(s"Size: ${costAndPositions._2.size}")

}
