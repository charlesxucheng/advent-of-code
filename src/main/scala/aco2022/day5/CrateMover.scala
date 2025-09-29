package aoc
package aco2022.day5

import aco2022.day5.CrateMover.{moveCratesAllAtOnce, moveCratesOneByOne}
import common.Utils.loadData

object CrateMover {

  def parseInput(
      lines: Iterable[String]
  ): (List[List[Char]], List[Instruction]) = {
    val input = lines.toList
    val (part1, part2) = input.splitAt(input.indexWhere(_.trim.isEmpty))
    (parseCratesInput(part1), parseInstructionsInput(part2.drop(1)))
  }

  private def parseCratesInput(lines: Iterable[String]): List[List[Char]] = {
    val linesList = lines.toList
    val numberOfStacks =
      linesList.find(_.trim.startsWith("1")).get.trim.split("\\s+").length
    val stackLines = linesList.takeWhile(line => !line.trim.startsWith("1"))

    val maxLength = stackLines.map(_.length).max
    val strings = stackLines.map(_.padTo(maxLength, ' '))
    val stacks = strings.map(parseLine).reverse.transpose.map(_.flatten.reverse)

    stacks
  }

  private def parseLine(line: String): List[Option[Char]] = {
    val charPositions = (1 until line.length by 4).toList
    charPositions.map { pos =>
      val char = line.charAt(pos)
      if char != ' ' then Some(char) else None
    }
  }

  private def parseInstructionsInput(
      lines: Iterable[String]
  ): List[Instruction] = {
    val pattern = """move (\d+) from (\d+) to (\d+)""".r
    lines.map { case pattern(numberOfCrates, from, to) =>
      Instruction(numberOfCrates.toInt, from.toInt, to.toInt)
    }.toList
  }

  private def moveCrates(
      stacks: List[List[Char]],
      instruction: Instruction,
      oneByeOne: Boolean
  ): List[List[Char]] = {
    val from = instruction.from - 1
    val to = instruction.to - 1
    val cratesTemp = stacks(from).take(instruction.numberOfCrates)
    val crates = if (oneByeOne) cratesTemp.reverse else cratesTemp
    val newStacks =
      stacks.updated(from, stacks(from).drop(instruction.numberOfCrates))
    newStacks.updated(to, crates ++ newStacks(to))
  }

  def moveCratesOneByOne(stacks: List[List[Char]], instructions: List[Instruction]): List[List[Char]] =
    instructions.foldLeft(stacks)(moveCrates(_, _, true))

  def moveCratesAllAtOnce(stacks: List[List[Char]], instructions: List[Instruction]): List[List[Char]] =
    instructions.foldLeft(stacks)(moveCrates(_, _, false))

  case class Instruction(numberOfCrates: Int, from: Int, to: Int)
}

@main def main(): Unit = {

  val filename = "aoc2022-day5-input.txt"
//  val filename = "test.txt"
  val (stacks, instructions) = loadData(filename)(CrateMover.parseInput)

  val part1ResultStacks = moveCratesOneByOne(stacks, instructions)
  val part2ResultStacks = moveCratesAllAtOnce(stacks, instructions)

  println(s"Part 1 result: ${part1ResultStacks.map(_.head).mkString}")
  println(s"Part 2 result: ${part2ResultStacks.map(_.head).mkString}")

}
