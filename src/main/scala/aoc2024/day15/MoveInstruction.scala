package aoc
package aoc2024.day15

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
