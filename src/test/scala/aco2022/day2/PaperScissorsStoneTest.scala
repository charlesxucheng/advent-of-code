package aoc
package aco2022.day2

import aco2022.day2.Outcome.*
import aco2022.day2.PaperScissorsStone.*
import aco2022.day2.{
  PaperScissorsStoneMapper,
  PaperScissorsStoneScoreCalculator
}

import aoc2022.UnitSpec
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

  "Given a desired outcome and the first shape of a game, then the second shape can be calculated correctly" in {
    Paper.findShapeForOutcome(Win) shouldBe Scissors
    Paper.findShapeForOutcome(Lose) shouldBe Stone
    Paper.findShapeForOutcome(Draw) shouldBe Paper
    Scissors.findShapeForOutcome(Win) shouldBe Stone
    Scissors.findShapeForOutcome(Lose) shouldBe Paper
    Scissors.findShapeForOutcome(Draw) shouldBe Scissors
    Stone.findShapeForOutcome(Win) shouldBe Paper
    Stone.findShapeForOutcome(Lose) shouldBe Scissors
    Stone.findShapeForOutcome(Draw) shouldBe Stone
  }

  "Values are mapped correctly under the second mapping scheme in Part 2" in {
    PaperScissorsStoneMapper.fromStringCol2Part2('A','Y') shouldBe (Draw, Stone)
    PaperScissorsStoneMapper.fromStringCol2Part2('B','Z') shouldBe (Win, Scissors)
    PaperScissorsStoneMapper.fromStringCol2Part2('C','X') shouldBe (Lose, Paper)
  }

  "Part 2: core is calculated correctly for a single game for" in {
    PaperScissorsStoneScoreCalculator.calculateScorePart2(List((Draw, Paper))) shouldBe List(5)
  }

  "Part 2: Total score is calculated correctly for multiple games" in {
    val input = List(
      (Win, Paper),
      (Draw, Stone),
      (Lose, Scissors)
    )
    val expected = List(8, 4, 3)
    PaperScissorsStoneScoreCalculator.calculateTotalScorePart2(input) shouldBe expected.sum
  }

}
