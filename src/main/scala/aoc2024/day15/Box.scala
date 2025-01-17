package aoc
package aoc2024.day15

import aoc2024.day15.Box.*
import common.Position

sealed trait Box {
  def move(instruction: MoveInstruction): Box
}
case class SingleCellBox(position: Position) extends Box {
  override def move(instruction: MoveInstruction): Box = SingleCellBox(
    this.position.shiftPosition(instruction)
  )
}

object Box {
  extension (position: Position) {
    def shiftPosition(instruction: MoveInstruction): Position =
      instruction match {
        case MoveInstruction.Left  => position.copy(x = position.x - 1)
        case MoveInstruction.Right => position.copy(x = position.x + 1)
        case MoveInstruction.Up    => position.copy(y = position.y - 1)
        case MoveInstruction.Down  => position.copy(y = position.y + 1)
      }
  }
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
