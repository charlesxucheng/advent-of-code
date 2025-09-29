package aoc
package aoc2024.day13

import aoc2024.day13.ClawMachine./?
import common.Utils.loadData
import scala.annotation.targetName
import scala.util.matching.Regex

object ClawMachine {
  def parseInput(input: Iterable[String]): Seq[ClawMachine] = {
    val buttonPattern: Regex = """Button ([AB]): X\+(\d+), Y\+(\d+)""".r
    val prizePattern: Regex = """Prize: X=(\d+), Y=(\d+)""".r

    val machines = input.to(LazyList)
      .filter(_ != "")
      .grouped(3)
      .flatMap {
        case Seq(
              buttonA @ buttonPattern("A", x1, y1),
              buttonB @ buttonPattern("B", x2, y2),
              prize @ prizePattern(x, y)
            ) =>
          Some(
            ClawMachine(
              Button(x1.toLong, y1.toLong),
              Button(x2.toLong, y2.toLong),
              Prize(x.toLong, y.toLong)
            )
          )
        case _ => None
      }
      .toSeq

    machines
  }

  extension (a: Long) {
    @targetName("safeDivide")
    infix def /?(b: Long): Option[Long] =
      Option.when(b != 0 && a % b == 0)(a / b)
  }
}

case class Button(x: Long, y: Long)

case class Prize(x: Long, y: Long)

case class ClawMachine(
    buttonA: Button,
    buttonB: Button,
    prize: Prize
) {
  // Reference: https://en.wikipedia.org/wiki/Cramer%27s_rule
  // a1 = buttonA.x, a2 = buttonA.y, b1 = buttonB.x, b2 = buttonB.y, c1 = prize.x, c2 = prize.y
  def solve(): Option[Long] = for
    numOfA <-
      (prize.x * buttonB.y - buttonB.x * prize.y) /? (buttonA.x * buttonB.y - buttonB.x * buttonA.y)
    numOfB <-
      (buttonA.x * prize.y - prize.x * buttonA.y) /? (buttonA.x * buttonB.y - buttonB.x * buttonA.y)
  yield numOfA * 3 + numOfB
}

@main def main(): Unit = {

  val filename = "aoc2024-day13-input.txt"
//  val filename = "test.txt"

  val clawMachines = loadData(filename)(ClawMachine.parseInput)

  println(clawMachines)

  val tokens = clawMachines.map(_.solve())
  println(tokens)
  println(s"Part 1 - Tokens Required: ${tokens.flatten.sum}")

  val offset = 10000000000000L
  val part2Machines = clawMachines.map(m =>
    m.copy(prize = Prize(m.prize.x + offset, m.prize.y + offset))
  )
  val part2Tokens = part2Machines.map(_.solve())
  println(part2Tokens)
  println(s"Part 2 - Tokens Required: ${part2Tokens.flatten.sum}")

}
