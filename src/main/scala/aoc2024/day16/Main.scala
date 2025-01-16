package aoc
package aoc2024.day16

import aoc2024.day16.ReindeerMaze.*
import common.Utils.loadData
import common.{Direction, Position, TwoDMap}
import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

case class QueueElement(position: Position, direction: Direction, cost: Long)

implicit val ordering: Ordering[QueueElement] =
  Ordering.by[QueueElement, Long](_.cost).reverse

case class ReindeerMaze(
    startPosition: Position,
    endPosition: Position,
    walls: Seq[Position]
) {
  def shortestPathCost(): Long = {
    shortestPath(
      Set.empty,
      PriorityQueue(QueueElement(startPosition, Direction.East, 0))
    )
  }

  @tailrec
  private def shortestPath(
      visited: Set[(Position, Direction)],
      paths: PriorityQueue[QueueElement]
  ): Long = {
    val currentElement = paths.dequeue()

    scribe.debug(s"Visiting $currentElement. Visited $visited")

    if (currentElement.position == endPosition)
      currentElement.cost
    else {
      val nextPositions = currentElement.position.cardinalPositions.filter {
        !walls.contains(_) && !visited.contains(
          (currentElement.position, currentElement.direction)
        )
      }.toSeq

      val nextMoves = nextPositions.map(nextPosition => {
        val moveDirection = currentElement.position.directionTo(nextPosition)
        val costToTurn = currentElement.direction.costToTurnTo(moveDirection)

        QueueElement(
          nextPosition,
          moveDirection,
          currentElement.cost + costToTurn + 1
        )
      })

      nextMoves.foreach(m => paths.enqueue(m))
      shortestPath(
        visited + ((currentElement.position, currentElement.direction)),
        paths
      )
    }
  }
}

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

  extension (direction: Direction) {
    def costToTurnTo(other: Direction): Int = {
      if (direction == other) 0
      else if (direction.turnRight == other || direction.turnLeft == other) 1000
      else 2000
    }
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
  scribe.info(s"Shortest path cost: ${maze.shortestPathCost()}")

}
