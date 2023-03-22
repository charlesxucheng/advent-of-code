package aoc2022
package day2

import PaperScissorsStone._
import Outcome._
import org.scalatest.matchers.should.Matchers.shouldBe
class PaperScissorsStoneTest extends UnitSpec {
  "Paper wins over stone" in {
    Paper vs Stone shouldBe Win
    Stone vs Paper shouldBe Lose
  }

  "Scissor wins over paper" in {
    Scissors vs Paper shouldBe Win
    Paper vs Scissors shouldBe Lose
  }

  "Stone wins over scissor" in {
    Stone vs Scissors shouldBe Win
    Scissors vs Stone shouldBe Lose
  }

  "Same items are a draw" in {
    Paper vs Paper shouldBe Draw
    Scissors vs Scissors shouldBe Draw
    Stone vs Stone shouldBe Draw
  }

  "Values are mapped correctly under the first mapping scheme" in {
    PaperScissorsStoneMapper.fromStringCol1('A') shouldBe Stone
    PaperScissorsStoneMapper.fromStringCol1('B') shouldBe Paper
    PaperScissorsStoneMapper.fromStringCol1('C') shouldBe Scissors
  }

  "Invalid values causes an exception under the first mapping scheme" in {
    assertThrows[IllegalArgumentException] {
      PaperScissorsStoneMapper.fromStringCol1('D')
    }
  }

  "Values are mapped correctly under the second mapping scheme" in {
    PaperScissorsStoneMapper.fromStringCol2('X') shouldBe Stone
    PaperScissorsStoneMapper.fromStringCol2('Y') shouldBe Paper
    PaperScissorsStoneMapper.fromStringCol2('Z') shouldBe Scissors
  }

  "Invalid values causes an exception under the second mapping scheme" in {
    assertThrows[IllegalArgumentException] {
      PaperScissorsStoneMapper.fromStringCol2('D')
    }
  }

  "Single input line can be mapped to a pair of items" in {
    PaperScissorsStoneScoreCalculator.parseInput(Iterator("A X")) shouldBe List((Stone, Stone))
  }

  "Multiple input lines can be mapped to list of items" in {
    val input = Iterator("A Y", "B X", "C Z")
    val expected = List((Stone, Paper), (Paper, Stone), (Scissors, Scissors))
    PaperScissorsStoneScoreCalculator.parseInput(input) shouldBe expected
  }

  "Score is calculated correctly for a single game" in {
    PaperScissorsStoneScoreCalculator.calculateScore(List((Stone, Paper))) shouldBe List(8)
  }

  "Score is calculated correctly for multiple games" in {
    val input = List(
      (Stone, Paper),
      (Paper, Stone),
      (Scissors, Scissors)
    )
    val expected = List(8, 1, 6)
    PaperScissorsStoneScoreCalculator.calculateScore(input) shouldBe expected
  }

  "Total score is calculated correctly for multiple games" in {
    val input = List(
      (Stone, Scissors),
      (Paper, Scissors),
      (Scissors, Scissors)
    )
    val expected = List(3, 9, 6).sum
    PaperScissorsStoneScoreCalculator.calculateTotalScore(input) shouldBe expected
  }
}
