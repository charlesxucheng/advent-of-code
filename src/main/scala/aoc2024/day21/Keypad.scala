package aoc
package aoc2024.day21

import common.Position

object Keypad {

  private val moveCosts = collection.mutable.Map
    .empty[(Position, Position, Int, Int), Long]
  private val numericKeypad: Map[Char, Position] = Map(
    '7' -> Position(0, 0),
    '8' -> Position(1, 0),
    '9' -> Position(2, 0),
    '4' -> Position(0, 1),
    '5' -> Position(1, 1),
    '6' -> Position(2, 1),
    '1' -> Position(0, 2),
    '2' -> Position(1, 2),
    '3' -> Position(2, 2),
    '0' -> Position(1, 3),
    'A' -> Position(2, 3)
  )
  private val directionalKeypad2: Map[Char, Position] = Map(
    '^' -> Position(1, 0),
    'A' -> Position(2, 0),
    '<' -> Position(0, 1),
    'v' -> Position(1, 1),
    '>' -> Position(2, 1)
  )
  private val numericKeypadPositions: Set[Position] = numericKeypad.values.toSet
  private val directionalKeypadPositions: Set[Position] =
    directionalKeypad2.values.toSet

  def bestMoveSequenceCost(
      keySequence: String,
      currentLevel: Int,
      maxLevel: Int
  ): Long = {
    ("A" + keySequence)
      .sliding(2)
      .map(_.toCharArray)
      .map(c => bestSingleMoveCost(c(0), c(1), currentLevel, maxLevel))
      .sum
  }

  private def bestSingleMoveCost(
      from: Char,
      to: Char,
      currentLevel: Int,
      maxLevel: Int
  ): Long = {

    val positionMap =
      if (currentLevel == 0) numericKeypad else directionalKeypad2

    val fromPosition = positionMap(from)
    val toPosition = positionMap(to)

    moveCosts.getOrElseUpdate(
      (fromPosition, toPosition, currentLevel, maxLevel), {
        val moveSequence =
          bestKeypadSequence(fromPosition, toPosition, currentLevel)
        if (currentLevel == maxLevel)
          moveSequence.length
        else
          bestMoveSequenceCost(
            moveSequence,
            currentLevel + 1,
            maxLevel
          )
      }
    )
  }

  private def bestKeypadSequence(
      fromPosition: Position,
      toPosition: Position,
      currentLevel: Int
  ): String = {

    val positions =
      if (currentLevel == 0) numericKeypadPositions
      else directionalKeypadPositions

    val deltaX = toPosition.x - fromPosition.x
    val deltaY = toPosition.y - fromPosition.y

    val horizontalDirection = if (deltaX > 0) ">" else "<"
    val verticalDirection = if (deltaY > 0) "v" else "^"
    val horizontalMovements = horizontalDirection * deltaX.abs
    val verticalMovements = verticalDirection * deltaY.abs

    def shouldGoVerticalFirst = {
      !positions(
        fromPosition.copy(x = fromPosition.x + deltaX)
      ) || (positions(
        fromPosition.copy(y = fromPosition.y + deltaY)
      ) && deltaX > 0)
    }

    val moveInstructions: String =
      if (shouldGoVerticalFirst)
        verticalMovements + horizontalMovements
      else
        horizontalMovements + verticalMovements

    moveInstructions + 'A'
  }

}
