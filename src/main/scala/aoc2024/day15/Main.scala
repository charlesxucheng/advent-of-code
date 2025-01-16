package aoc
package aoc2024.day15

import aoc2024.day15.WarehouseMap.*
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

sealed trait Box {
  def move(instruction: MoveInstruction): Box
}

case class SingleCellBox(position: Position) extends Box {
  override def move(instruction: MoveInstruction): Box = SingleCellBox(
    this.position.shiftPosition(instruction)
  )
}

case class DoubleCellBox(firstPos: Position, secondPos: Position) extends Box {
  override def move(instruction: MoveInstruction): Box = DoubleCellBox(
    firstPos.shiftPosition(instruction),
    secondPos.shiftPosition(instruction)
  )

  def getTargetPositions(instruction: MoveInstruction): Set[Position] =
    Set(firstPos, secondPos).map(_.shiftPosition(instruction)) -- Set(
      firstPos,
      secondPos
    )

  def measuringPosition: Position =
    if (firstPos.x == secondPos.x) {
      if (firstPos.y < secondPos.y) firstPos else secondPos
    } else {
      if (firstPos.x < secondPos.x) firstPos else secondPos
    }
}

case class WarehouseMap(
    wallPositions: Set[Position],
    boxes: Set[Box],
    robotPosition: Position
) {
  private val boxPositions: Set[Position] = boxes.flatMap {
    case SingleCellBox(position)      => Set(position)
    case DoubleCellBox(first, second) => Set(first, second)
  }

  assert(!(isWall(robotPosition) || isBox(robotPosition)))

  private def from(position: Position): Option[Box] =
    boxes.find(box =>
      box match {
        case single: SingleCellBox => single.position == position
        case double: DoubleCellBox =>
          double.firstPos == position || double.secondPos == position
      }
    )

  def gpsCoordinate: Long = boxes.toSeq.map {
    case SingleCellBox(p) => p.y * 100L + p.x
    case b: DoubleCellBox =>
      b.measuringPosition.y * 100L + b.measuringPosition.x
  }.sum

  def moveRobot(instruction: MoveInstruction): WarehouseMap = {
    scribe.debug(
      s"New Step: Robot position $robotPosition Move Instruction: $instruction"
    )
    scribe.debug(s"Boxes: $boxes")
    moveRobotRec(
      robotPosition,
      Set(robotPosition.shiftPosition(instruction)),
      instruction,
      Set.empty
    )
  }

  @tailrec
  private def moveRobotRec(
      robotPosition: Position,
      targetPositions: Set[Position],
      instruction: MoveInstruction,
      accumulatedBoxes: Set[Box]
  ): WarehouseMap = {
    scribe.debug(
      s"Target Positions are ${targetPositions.map(_.displayAsArrayElement)}"
    )

    if (targetPositions.exists(isWall)) {
      val wallPositions = targetPositions.filter(isWall)
      scribe.debug(s"Positions $wallPositions are wall. Map unchanged.")
      this
    } else if (targetPositions.exists(isBox)) {
      val boxPositions = targetPositions.filter(isBox)
      scribe.debug(
        s"Positions $boxPositions are boxes. Checking what are behind the boxes"
      )

      val boxes = targetPositions.flatMap(from)
      val newTargetPositions = boxes.flatMap {
        case SingleCellBox(p)   => Set(p.shiftPosition(instruction))
        case dcb: DoubleCellBox => dcb.getTargetPositions(instruction)
      }

      moveRobotRec(
        robotPosition,
        newTargetPositions,
        instruction,
        accumulatedBoxes ++ boxes
      )
    } else if (targetPositions.forall(isSpace)) {
      scribe.debug("All positions are space. Moving robot and any boxes accumulated")
      val newBoxes = accumulatedBoxes.toSeq.map(_.move(instruction))
      WarehouseMap(
        wallPositions,
        boxes -- accumulatedBoxes ++ newBoxes,
        robotPosition.shiftPosition(instruction)
      )
    } else {
      throw IllegalStateException("Another Robot detected on the map!!")
    }
  }

  private def isSpace(position: Position) =
    !isWall(position) && !isBox(position) && !isRobot(position)

  private def isWall(position: Position) = wallPositions.contains(position)

  private def isBox(position: Position) = boxPositions.contains(position)

  private def isRobot(position: Position) = robotPosition == position

}

case class Robot(position: Position, map: WarehouseMap) {}

object WarehouseMap {
  def apply(map: TwoDMap[Char]): WarehouseMap = {
    val wallPositions: Set[Position] = map.findAll('#')
    val singleCellBoxes = map.findAll('O').map(SingleCellBox.apply)
    val doubleCellBoxes = map
      .findAll('[')
      .map(b => DoubleCellBox(b, b.copy(x = b.x + 1)))

    val robotPosition: Position = map.findAll('@').head
    WarehouseMap(
      wallPositions,
      singleCellBoxes ++ doubleCellBoxes,
      robotPosition
    )
  }

  extension (position: Position) {
    def shiftPosition(instruction: MoveInstruction): Position =
      instruction match {
        case MoveInstruction.Left  => position.copy(x = position.x - 1)
        case MoveInstruction.Right => position.copy(x = position.x + 1)
        case MoveInstruction.Up    => position.copy(y = position.y - 1)
        case MoveInstruction.Down  => position.copy(y = position.y + 1)
      }
  }

  extension (map: TwoDMap[Char]) {
    def widen(): TwoDMap[Char] = {
      val array = map.map
      val outArray = array.map(row =>
        row.flatMap(c =>
          c match {
            case 'O'     => Seq('[', ']')
            case '@'     => Seq('@', '.')
            case a: Char => Seq(a, a)
          }
        )
      )
      TwoDMap(outArray)
    }
  }
}

@main def main(): Unit = {

  val mapFilename = "aoc2024-day15-input1.txt"
//  val mapFilename = "test.txt"
  val instructionsFilename = "aoc2024-day15-input2.txt"
//  val instructionsFilename = "test2.txt"

  val map = loadData(mapFilename)(TwoDMap.parseInput(identity))
  map.display()
  val warehouseMap = WarehouseMap(map)
  scribe.info(
    s"Robot position: ${warehouseMap.robotPosition.displayAsArrayElement}"
  )

  val instructions = loadData(instructionsFilename)(MoveInstruction.parseInput)
  scribe.info(instructions.toString())

  val resultMap = instructions.foldLeft(warehouseMap)((map, instruction) => {
    map.moveRobot(instruction)
  })

  scribe.info(s"GPS Coordinate of all boxes = ${resultMap.gpsCoordinate}")

  // Part 2
  scribe.info("Part 2")
  val widenedMap = map.widen()
  widenedMap.display()

  val warehouseMap2 = WarehouseMap(widenedMap)
  scribe.info(
    s"Robot position: ${warehouseMap2.robotPosition.displayAsArrayElement}"
  )

  val resultMap2 = instructions.foldLeft(warehouseMap2)((map, instruction) => {
    map.moveRobot(instruction)
  })

  scribe.info(s"GPS Coordinate of all boxes = ${resultMap2.gpsCoordinate}")
}
