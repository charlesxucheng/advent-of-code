package aoc
package common

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

  override def toString: String = s"(x:$x, y:$y)"
  def displayAsArrayElement: String = s"(Row:$y, Col:$x)"
}
