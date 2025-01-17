package aoc
package aoc2024.day16

import aoc2024.day16.ReindeerMaze.*
import common.Utils.loadData
import common.{Direction, Position, TwoDMap}
import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

case class Move(position: Position, direction: Direction, cost: Long)

case class QueueElement(
    position: Position,
    direction: Direction,
    cost: Long,
    visited: Set[Position]
)

implicit val ordering: Ordering[QueueElement] =
  Ordering.by[QueueElement, Long](_.cost).reverse

object ReindeerMaze {
  def createMap(map: TwoDMap[Char]): ReindeerMaze = {
    val startPosition = map.findFirstPosition('S').get
    val endPosition = map.findFirstPosition('E').get
    val walls = map.findAll('#').toSeq
    ReindeerMaze(
      startPosition,
      endPosition,
      walls
    )
  }

  private def nextMoves(position: Position, direction: Direction): Seq[Move] =
    Seq(
      Move(position.shift(direction), direction, 1),
      Move(position, direction.turnLeft, 1000),
      Move(position, direction.turnRight, 1000)
    )
}

case class ReindeerMaze(
    startPosition: Position,
    endPosition: Position,
    walls: Seq[Position]
) {
  def shortestPathCostAndPositions(): (Long, Set[Position]) = {
    shortestPath(
      Set.empty,
      PriorityQueue(
        QueueElement(startPosition, Direction.East, 0, Set(startPosition))
      )
    )
  }

  @tailrec
  private def shortestPath(
      visited: Set[(Position, Direction)],
      paths: PriorityQueue[QueueElement]
  ): (Long, Set[Position]) = {
    val currentElement = paths.dequeue()

    scribe.debug(
      s"Visiting (${currentElement.position},${currentElement.direction},${currentElement.cost})."
    )

    if (currentElement.position == endPosition) {
      val positions =
        getPositionsForAllShortestPaths(paths, currentElement.cost, endPosition)
      (currentElement.cost, positions ++ currentElement.visited + endPosition)

    } else {
      val nextQueueElements = ReindeerMaze
        .nextMoves(currentElement.position, currentElement.direction)
        .filter { move =>
          !walls.contains(move.position) && !visited.contains(
            (move.position, move.direction)
          )
        }
        .map(nextMove => {
          QueueElement(
            nextMove.position,
            nextMove.direction,
            currentElement.cost + nextMove.cost,
            currentElement.visited + currentElement.position
          )
        })

      nextQueueElements.foreach(m => paths.enqueue(m))
      shortestPath(
        visited + ((currentElement.position, currentElement.direction)),
        paths
      )
    }
  }

  private def getPositionsForAllShortestPaths(
      paths: PriorityQueue[QueueElement],
      cost: Long,
      endPosition: Position
  ): Set[Position] = {
    paths
      .filter(e => e.position == endPosition && e.cost == cost)
      .map(_.visited)
      .foldLeft(Set.empty[Position])(_ ++ _)
  }
}

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
