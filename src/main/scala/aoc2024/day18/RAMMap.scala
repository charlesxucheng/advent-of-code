package aoc
package aoc2024.day18

import common.Position
import scala.annotation.tailrec
import scala.collection.mutable.PriorityQueue

case class Move(position: Position, time: Int)

implicit val ordering: Ordering[Move] = Ordering.by[Move, Int](_.time).reverse

object RAMMap {

  def parseInput(input: Iterator[String]): Seq[Position] = {
    input
      .map(x => {
        val pair = x.split(",")
        Position(pair(0).toInt, pair(1).toInt)
      })
      .toSeq
  }

  def apply(colSize: Int, rowSize: Int, corruptions: Seq[Position]): RAMMap = {
    RAMMap(
      colSize,
      rowSize,
      Position(0, 0),
      Position(colSize - 1, rowSize - 1),
      corruptions
    )
  }

  def nextMoves(
      position: Position,
      time: Int,
      map: RAMMap,
      visited: Set[Position]
  ): Seq[Position] =
    position.cardinalPositions.filter { p =>
      map.contains(p) && !visited.contains(p) && !map.corruptions.contains(p)
    }.toSeq

  def shortestPath(map: RAMMap): Int =
    shortestPathRec(
      map,
      Set.empty,
      PriorityQueue(Move(map.startPosition, 1))
    ) - 1

  // Performs a binary search from the 1025th memory corruption to the end
  @tailrec
  def allPathBlockedAt(
      mapWithAllCorruptions: RAMMap,
      min: Int,
      max: Int
  ): Int = {
    val numberToTry = (min + max) / 2
    val mapToTry = mapWithAllCorruptions.copy(corruptions =
      mapWithAllCorruptions.corruptions.take(numberToTry)
    )

    if (hasShortestPath(mapToTry)) {
      if (max - min <= 1)
        throw Exception(
          "Searched all inputs and cannot find a corruption position that will cause all paths to be blocked"
        )
      else
        allPathBlockedAt(mapWithAllCorruptions, numberToTry + 1, max)
    } else {
      if (max - min <= 1)
        numberToTry - 1
      else
        allPathBlockedAt(mapWithAllCorruptions, min, numberToTry - 1)
    }
  }

  private def hasShortestPath(map: RAMMap): Boolean =
    try {
      shortestPath(map)
      true
    } catch {
      case _: NoSuchElementException => false
    }

  @tailrec
  private def shortestPathRec(
      map: RAMMap,
      visited: Set[Position],
      nextMoves: PriorityQueue[Move]
  ): Int = {
    val currentMove = nextMoves.dequeue()

    scribe.debug(s"Visiting ($currentMove).")

    if (currentMove.position == map.endPosition)
      currentMove.time
    else {
      val nextQueueElements = RAMMap
        .nextMoves(
          currentMove.position,
          currentMove.time + 1,
          map,
          visited + currentMove.position
        )
        .map(nextPosition => {
          Move(
            nextPosition,
            currentMove.time + 1
          )
        })

      nextQueueElements.foreach(m =>
        if (!nextMoves.exists(m.position == _.position)) {
          scribe.debug(s"Adding $m")
          nextMoves.enqueue(m)
        }
      )

      shortestPathRec(
        map,
        visited + currentMove.position,
        nextMoves
      )
    }
  }
}

case class RAMMap(
    colSize: Int,
    rowSize: Int,
    startPosition: Position,
    endPosition: Position,
    corruptions: Seq[Position]
) {
  require(corruptions.forall(this.contains))

  def contains(position: Position): Boolean =
    position.x >= 0 && position.x < colSize && position.y >= 0 && position.y < rowSize
}
