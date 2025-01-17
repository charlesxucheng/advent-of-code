package aoc
package common

import scala.annotation.targetName

// x = col, y = row
case class Position(x: Int, y: Int) {
  def cardinalPositions: Set[Position] = {
    Set(
      Position(x, y - 1),
      Position(x + 1, y),
      Position(x, y + 1),
      Position(x - 1, y)
    )
  }

  def neighboringPositions: Set[Position] =
    (x - 1 to x + 1)
      .flatMap(i => (y - 1 to y + 1).map(j => Position(i, j)))
      .filterNot(p => p == this)
      .toSet

  def shift(direction: Direction): Position = direction match {
    case Direction.North => Position(x, y - 1)
    case Direction.East  => Position(x + 1, y)
    case Direction.South => Position(x, y + 1)
    case Direction.West  => Position(x - 1, y)
  }

  @targetName("minus")
  def -(other: Position): (Int, Int) = (x - other.x, y - other.y)

  override def toString: String = s"(x:$x, y:$y)"
  def displayAsArrayElement: String = s"(Row:$y, Col:$x)"
}
