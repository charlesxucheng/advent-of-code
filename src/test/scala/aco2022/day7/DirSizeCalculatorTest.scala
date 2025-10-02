package aoc
package aco2022.day7

import aco2022.day7.DirSizeCalculator.*

import aoc2022.UnitSpec
import org.scalatest.matchers.should.Matchers.*

class DirSizeCalculatorTest extends UnitSpec {
  "DirSizeCalculator" should {
    "calculate size of an empty dir to be 0" in {
      val input = List(
        List(
          "$ cd /",
          "$ ls"
        ),
        List(
          "$ cd /",
          "$ ls",
          "dir a",
          "$ cd a",
          "$ ls",
          "dir b",
          "$ cd b",
          "$ ls"
        ),
        List(
          "$ cd /",
          "$ ls",
          "dir a",
          "0 a.txt",
          "$ cd a",
          "$ ls",
          "0 b.txt"
        )
      )
      input.foreach(i =>
        calculateDirectorySizes(parseInput(i)).values.sum shouldBe 0
      )
    }

    "calculate size of a dir with only file(s) to be the size of the file" in {
      val input = Table(
        ("Input", "Expected Size"),
        (
          List(
            "$ cd /",
            "$ ls",
            "101 a.txt"
          ),
          101
        ),
        (
          List(
            "$ cd /",
            "$ ls",
            "101 a",
            "102 b"
          ),
          203
        ),
        (
          List(
            "$ cd /",
            "$ ls",
            "101 a",
            "102 b",
            "10302446 c"
          ),
          101 + 102 + 10302446
        ),
      )

      forAll(input) { (input, expectedSize) =>
        calculateDirectorySizes(
          parseInput(input)
        ).values.sum shouldBe expectedSize
      }
    }

    "calculate size of a dir with subdir(s) to be the sum of the size of the subdirs" in {
      val testData = Table(
        ("Input", "Expected Size"),
        (
          List(
            "$ cd /",
            "$ ls",
            "dir a",
            "$ cd a",
            "$ ls",
            "123 file.txt"
          ),
          123 + 123
        ),
        (
          List(
            "$ cd /",
            "$ ls",
            "dir a",
            "$ cd a",
            "$ ls",
            "100 file1.txt",
            "200 file2.txt"
          ),
          300 + 300
        ),
        (
          List(
            "$ cd /",
            "$ ls",
            "dir a",
            "dir b",
            "$ cd a",
            "$ ls",
            "100 file1.txt",
            "$ cd ..",
            "$ cd b",
            "$ ls",
            "200 file2.txt"
          ),
          300 + 100 + 200
        ),
        (
          List(
            "$ cd /",
            "$ ls",
            "dir a",
            "10 file.root",
            "$ cd a",
            "$ ls",
            "dir b",
            "20 file.a",
            "$ cd b",
            "$ ls",
            "30 file.b"
          ),
          (10 + 20 + 30) + (20 + 30) + 30
        )
      )

      forAll(testData) { (input, expectedSize) =>
        calculateDirectorySizes(
          parseInput(input)
        ).values.sum shouldBe expectedSize
      }
    }
  }

}
