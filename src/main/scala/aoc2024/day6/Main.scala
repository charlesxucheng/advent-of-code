package aoc
package aoc2024.day6

import aoc2024.day6.MazeWalker.tryAddObstacle
import common.Direction.{East, North, South, West}
import common.Utils.loadData
import common.{Direction, Position, TwoDMap, TwoDimensionalArray}
import scala.annotation.tailrec

object MazeWalker {

//  private def onMapBoarder(
//      position: Position,
//      map: Array[Array[Char]]
//  ): Boolean =
//    position.x == 0 || position.x == map.head.length - 1 || position.y == 0 || position.y == map.length - 1

  private def onObstacle(position: Position, map: TwoDMap[Char]): Boolean =
    map.hasValue(position, '#')

  private def isLoop(
      rightTurnPosition: Position,
      direction: Direction,
      previousRightTurnPositions: Set[(Position, Direction)]
  ) =
    previousRightTurnPositions.contains((rightTurnPosition, direction))

  @tailrec
  def forward(
      map: TwoDMap[Char],
      position: Position,
      direction: Direction,
      positionsCovered: Set[Position],
      rightTurnPositions: Set[(Position, Direction)]
  ): (Boolean, Set[Position]) = {

    require(!onObstacle(position, map))

    val offset = direction match {
      case North => (0, -1)
      case South => (0, 1)
      case East  => (1, 0)
      case West  => (-1, 0)
    }

    val newPosition = Position(position.x + offset._1, position.y + offset._2)
    if (onObstacle(newPosition, map)) {
      println(s"Turn right. New Position is $position")
      if (isLoop(position, direction, rightTurnPositions))
        (true, positionsCovered)
      else
        forward(
          map,
          position,
          direction.turnRight,
          positionsCovered,
          rightTurnPositions + ((position, direction))
        )
    } else if (map.isOnBorder(newPosition)) {
      println(s"Reached border. New Position is $position")
      (false, positionsCovered + newPosition)
    } else {
      println(s"Moving forward. New Position is $newPosition")
      forward(
        map,
        newPosition,
        direction,
        positionsCovered + newPosition,
        rightTurnPositions
      )
    }
  }

  def tryAddObstacle(
      map: TwoDMap[Char],
      obstacle: Position
  ): Option[TwoDMap[Char]] =
    if (map.get(obstacle) == '.') {
      Some(map.updated(obstacle, '#'))
    } else {
      None
    }
}

@main def main(): Unit = {

  val filename = "aoc2024-day6-input.txt"
//  val filename = "test.txt"
  val map = loadData(filename)(TwoDMap.parseInput(identity[Char]))

  val startLocation = map.findFirst('^')
  startLocation.foreach { case (row, col) =>
    println(s"Starting location is ($col, $row)")
    val result = MazeWalker.forward(
      map,
      Position(col, row),
      North,
      Set(Position(col, row)),
      Set.empty
    )
    println(
      s"Loop encountered: ${result._1}. The guard visited ${result._2.size} distinct positions"
    )
  }

  // Part 2
  val numberOfLoops = (for {
    i <- map.getColRange
    j <- map.getRowRange
  } yield {
    println(s"Checking loop for ($j, $i)")
    val count = tryAddObstacle(map, Position(j, i))
      .map(updatedMap => {
        val loc = updatedMap.findFirst('^').get
        val startPosition = Position(loc._2, loc._1)
        val result = MazeWalker
          .forward(
            updatedMap,
            startPosition,
            North,
            Set(startPosition),
            Set.empty
          )
          ._1
        println(s"Loop: $result")
        result
      })
    count
  }).flatten.count(_ == true)

  println(s"The number of that will cause loops is $numberOfLoops")

}
