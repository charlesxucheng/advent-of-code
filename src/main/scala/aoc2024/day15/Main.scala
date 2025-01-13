package aoc
package aoc2024.day15

import common.Utils.loadData
import common.{Position, TwoDMap}
import scala.annotation.tailrec

enum MoveInstruction(val symbol: Char) {
  case Left extends MoveInstruction('<')
  case Right extends MoveInstruction('>')
  case Up extends MoveInstruction('^')
  case Down extends MoveInstruction('v')
}

object MoveInstruction {
  def parseInput(input: Iterator[String]): Seq[MoveInstruction] =
    input.toSeq.flatten.flatMap(fromChar)

  private def fromChar(c: Char): Option[MoveInstruction] =
    MoveInstruction.values.find(_.symbol == c)
}

object WarehouseMap {
  def apply(map: TwoDMap[Char]): WarehouseMap = {
    val wallPositions: Set[Position] = map.findAll('#')
    val boxPositions: Set[Position] = map.findAll('O')
    val robotPosition: Position = map.findAll('@').head
    WarehouseMap(wallPositions, boxPositions, robotPosition)
  }
}

case class WarehouseMap(
    wallPositions: Set[Position],
    boxPositions: Set[Position],
    robotPosition: Position
) {
  require(!(isWall(robotPosition) || isBox(robotPosition)))

  def gpsCoordinate: Long = boxPositions.toSeq.map(p => p.y * 100L + p.x).sum

  def moveRobot(instruction: MoveInstruction): WarehouseMap =
    moveRobotRec(robotPosition, robotPosition, instruction, Set.empty)

  @tailrec
  private def moveRobotRec(
      robotPosition: Position,
      referencePosition: Position,
      instruction: MoveInstruction,
      accumulatedBoxes: Set[Position]
  ): WarehouseMap = {
    val newPosition = shiftPosition(referencePosition, instruction)
    println(s"Target Position is ${newPosition.displayAsArrayElement}")

    if (isWall(newPosition)) {
      println("Hit a wall. Map unchanged.")
      this
    } else if (isBox(newPosition)) {
      println("Hit a box. Checking what is behind the box")
      moveRobotRec(
        robotPosition,
        newPosition,
        instruction,
        accumulatedBoxes + newPosition
      )
    } else if (isSpace(newPosition)) {
      println("Hit a space. Moving robot and any boxes accumulated")
      val newBoxPositions = accumulatedBoxes.map(shiftPosition(_, instruction))
      WarehouseMap(
        wallPositions,
        boxPositions -- accumulatedBoxes ++ newBoxPositions,
        shiftPosition(robotPosition, instruction)
      )
    } else {
      throw IllegalStateException("Another Robot detected on the map!!")
    }
  }

  private def shiftPosition(position: Position, instruction: MoveInstruction) =
    instruction match {
      case MoveInstruction.Left  => position.copy(x = position.x - 1)
      case MoveInstruction.Right => position.copy(x = position.x + 1)
      case MoveInstruction.Up    => position.copy(y = position.y - 1)
      case MoveInstruction.Down  => position.copy(y = position.y + 1)
    }

  private def isWall(position: Position) = wallPositions.contains(position)

  private def isBox(position: Position) = boxPositions.contains(position)

  private def isRobot(position: Position) = robotPosition == position

  private def isSpace(position: Position) =
    !isWall(position) && !isBox(position) && !isRobot(position)
}

case class Robot(position: Position, map: WarehouseMap) {}

@main def main(): Unit = {

  val mapFilename = "aoc2024-day15-input1.txt"
//  val mapFilename = "test.txt"
  val instructionsFilename = "aoc2024-day15-input2.txt"
//  val instructionsFilename = "test2.txt"

  val map = loadData(mapFilename)(TwoDMap.parseInput(identity))
  val warehouseMap = WarehouseMap(map)
  map.display()
  println(
    s"Robot position: ${warehouseMap.robotPosition.displayAsArrayElement}"
  )

  val instructions = loadData(instructionsFilename)(MoveInstruction.parseInput)
  println(instructions)

  val resultMap = instructions.foldLeft(warehouseMap)((map, instruction) => {
    map.moveRobot(instruction)
  })

  println(s"GPS Coordinate of all boxes = ${resultMap.gpsCoordinate}")
}
