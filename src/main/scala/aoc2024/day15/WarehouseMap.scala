package aoc
package aoc2024.day15

import aoc2024.day15.Box.*
import common.{Position, TwoDMap}
import scala.annotation.tailrec

case class Robot(position: Position, map: WarehouseMap)

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

  private def from(position: Position): Option[Box] =
    boxes.find(box =>
      box match {
        case single: SingleCellBox => single.position == position
        case double: DoubleCellBox =>
          double.firstPos == position || double.secondPos == position
      }
    )

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
      scribe.debug(
        "All positions are space. Moving robot and any boxes accumulated"
      )
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
