package aoc
package aoc2024.day16

import aoc2024.day16.ReindeerMaze.*
import common.{Direction, Position, TwoDMap}
import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

case class Move(position: Position, direction: Direction, cost: Long)

case class QueueElement(
    move: Move,
    visited: Set[Position]
)

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
        QueueElement(Move(startPosition, Direction.East, 0), Set(startPosition))
      )
    )
  }

  @tailrec
  private def shortestPath(
      visited: Set[(Position, Direction)],
      paths: PriorityQueue[QueueElement]
  ): (Long, Set[Position]) = {
    val currentElement = paths.dequeue()
    val currentMove = currentElement.move

    scribe.debug(
      s"Visiting ($currentMove)."
    )

    if (currentMove.position == endPosition) {
      val positions =
        getPositionsForAllShortestPaths(paths, currentMove.cost, endPosition)
      (currentMove.cost, positions ++ currentElement.visited + endPosition)

    } else {
      val nextQueueElements = ReindeerMaze
        .nextMoves(currentMove.position, currentMove.direction)
        .filter { move =>
          !walls.contains(move.position) && !visited.contains(
            (move.position, move.direction)
          )
        }
        .map(nextMove => {
          QueueElement(
            Move(
              nextMove.position,
              nextMove.direction,
              currentMove.cost + nextMove.cost
            ),
            currentElement.visited + currentMove.position
          )
        })

      nextQueueElements.foreach(m => paths.enqueue(m))
      shortestPath(
        visited + ((currentMove.position, currentMove.direction)),
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
      .filter(e => e.move.position == endPosition && e.move.cost == cost)
      .map(_.visited)
      .foldLeft(Set.empty[Position])(_ ++ _)
  }
}
