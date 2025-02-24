package aoc
package aoc2024.day17

import aoc2024.day17.Computer.parseInput
import common.Utils.loadData

@main
def main(): Unit = {

  val filename = "aoc2024-day17-input.txt"
//  val filename = "test.txt"

  val computer = loadData(filename)(parseInput)
  println(computer.get.runUntilEnd())

  // Part 2
  println(s"The value in register A should be ${Computer.findNumberForPart2().head}")
}
