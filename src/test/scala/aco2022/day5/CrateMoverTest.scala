package aoc
package aco2022.day5

import aco2022.day5.CrateMover.Instruction

import aoc2022.UnitSpec
import org.scalatest.matchers.should.Matchers.*

class CrateMoverTest extends UnitSpec {
  "CrateMover" should {
    val input =
      """                [M]     [W] [M]
        |            [L] [Q] [S] [C] [R]
        |            [Q] [F] [F] [T] [N] [S]
        |    [N]     [V] [V] [H] [L] [J] [D]
        |    [D] [D] [W] [P] [G] [R] [D] [F]
        |[T] [T] [M] [G] [G] [Q] [N] [W] [L]
        |[Z] [H] [F] [J] [D] [Z] [S] [H] [Q]
        |[B] [V] [B] [T] [W] [V] [Z] [Z] [M]
        | 1   2   3   4   5   6   7   8   9
        |
        |move 1 from 7 to 4
        |move 1 from 6 to 2
        |move 5 from 9 to 4
        |""".stripMargin
    "load stacks info correctly" in {

      CrateMover
        .parseInput(input.split(System.lineSeparator()).toList)
        ._1 shouldBe List(
        List('T', 'Z', 'B'),
        List('N', 'D', 'T', 'H', 'V'),
        List('D', 'M', 'F', 'B'),
        List('L', 'Q', 'V', 'W', 'G', 'J', 'T'),
        List('M', 'Q', 'F', 'V', 'P', 'G', 'D', 'W'),
        List('S', 'F', 'H', 'G', 'Q', 'Z', 'V'),
        List('W', 'C', 'T', 'L', 'R', 'N', 'S', 'Z'),
        List('M', 'R', 'N', 'J', 'D', 'W', 'H', 'Z'),
        List('S', 'D', 'F', 'L', 'Q', 'M')
      )
    }

    "load instructions correctly" in {
      CrateMover
        .parseInput(input.split(System.lineSeparator()).toList)
        ._2 shouldBe
        List(
          Instruction(1, 7, 4),
          Instruction(1, 6, 2),
          Instruction(5, 9, 4)
        )
    }

    "move crates correctly" in {
      val (stacks, instructions) =
        CrateMover.parseInput(input.split(System.lineSeparator()).toList)
      CrateMover.moveCratesOneByOne(stacks, instructions) shouldBe
        List(
          "TZB",
          "SNDTHV",
          "DMFB",
          "QLFDSWLQVWGJT",
          "MQFVPGDW",
          "FHGQZV",
          "CTLRNSZ",
          "MRNJDWHZ",
          "M"
        ).map(_.toCharArray.toList)
    }
  }
}
