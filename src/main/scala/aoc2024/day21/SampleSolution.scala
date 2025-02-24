package aoc
package aoc2024.day21

import scala.io.Source
import scala.util.Using

// Reference solution from https://scalacenter.github.io/scala-advent-of-code/2024/puzzles/day21

def loadFileSync(path: String): String =
  Using.resource(Source.fromFile(path))(_.mkString)

val filename = "aoc2024-day21-input.txt"
def loadInput(): String = loadFileSync(filename)

case class Pos(x: Int, y: Int):
  def +(other: Pos) = Pos(x + other.x, y + other.y)
  def -(other: Pos) = Pos(x - other.x, y - other.y)
  def projX = Pos(x, 0)
  def projY = Pos(0, y)

val numericKeypad = Map(
  '7' -> Pos(0, 0), '8' -> Pos(1, 0), '9' -> Pos(2, 0),
  '4' -> Pos(0, 1), '5' -> Pos(1, 1), '6' -> Pos(2, 1),
  '1' -> Pos(0, 2), '2' -> Pos(1, 2), '3' -> Pos(2, 2),
  '0' -> Pos(1, 3), 'A' -> Pos(2, 3),
)
val numericKeypadPositions = numericKeypad.values.toSet

val directionalKeypad = Map(
  '^' -> Pos(1, 0), 'A' -> Pos(2, 0),
  '<' -> Pos(0, 1), 'v' -> Pos(1, 1), '>' -> Pos(2, 1),
)
val directionalKeypadPositions = directionalKeypad.values.toSet


/**********/
/* Part 1 */
/**********/

def minPathStep(from: Pos, to: Pos, positions: Set[Pos]): String =
  val shift = to - from
  val h = (if shift.x > 0 then ">" else "<") * shift.x.abs
  val v = (if shift.y > 0 then "v" else "^") * shift.y.abs
  val reverse = !positions(from + shift.projX) || (positions(from + shift.projY) && shift.x > 0)
  if reverse then v + h + 'A' else h + v + 'A'

def minPath(input: String, isNumeric: Boolean = false): String =
  val keypad = if isNumeric then numericKeypad else directionalKeypad
  val positions = if isNumeric then numericKeypadPositions else directionalKeypadPositions
  (s"A$input").map(keypad).sliding(2).map(p => minPathStep(p(0), p(1), positions)).mkString

def part1(input: String): Long =
  input
    .linesIterator
    .filter(_.nonEmpty)
    .map: line => // 029A
      val path1 = minPath(line, isNumeric = true) // <A^A^^>AvvvA
      val path2 = minPath(path1) // v<<A>>^A<A>A<AAv>A^A<vAAA^>A
      val path3 = minPath(path2) // <vA<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AA<vA>A^A<A>Av<<A>A^>AAA<Av>A^A
      val num = line.init.toLong // 29
      val len = path3.length() // 68
      len * num // 211930
    .sum

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")


/**********/
/* Part 2 */
/**********/

val cache = collection.mutable.Map.empty[(Pos, Pos, Int, Int), Long]
def minPathStepCost(from: Pos, to: Pos, level: Int, maxLevel: Int): Long =
  cache.getOrElseUpdate((from, to, level, maxLevel), {
    val positions = if level == 0 then numericKeypadPositions else directionalKeypadPositions
    val shift = to - from
    val h = (if shift.x > 0 then ">" else "<") * shift.x.abs
    val v = (if shift.y > 0 then "v" else "^") * shift.y.abs
    val reverse = !positions(from + shift.projX) || (positions(from + shift.projY) && shift.x > 0)
    val res = if reverse then v + h + 'A' else h + v + 'A'
    if level == maxLevel then res.length() else minPathCost(res, level + 1, maxLevel)
  })

def minPathCost(input: String, level: Int, maxLevel: Int): Long =
  val keypad = if level == 0 then numericKeypad else directionalKeypad
  (s"A$input").map(keypad).sliding(2).map(p => minPathStepCost(p(0), p(1), level, maxLevel)).sum

def part2(input: String): Long = {
  val costs = input
    .linesIterator
    .filter(_.nonEmpty)
    .map(line => minPathCost(line, 0, 3))

  println(costs.toSeq)

  input
    .linesIterator
    .filter(_.nonEmpty)
    .map(line => minPathCost(line, 0, 3) * line.init.toLong)
    .sum
}

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")