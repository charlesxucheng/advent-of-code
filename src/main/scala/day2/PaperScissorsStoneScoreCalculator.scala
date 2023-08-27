package aoc2022
package day2

import scala.io.Source
import PaperScissorsStone._
import PaperScissorsStoneMapper._

object PaperScissorsStoneMapper {
  def fromStringCol1(s: Char): PaperScissorsStone = s match
    case 'A' => PaperScissorsStone.Stone
    case 'B' => PaperScissorsStone.Paper
    case 'C' => PaperScissorsStone.Scissors
    case _ => throw new IllegalArgumentException(s"Invalid input: $s")

  def fromStringCol2(s: Char): PaperScissorsStone = s match
    case 'X' => PaperScissorsStone.Stone
    case 'Y' => PaperScissorsStone.Paper
    case 'Z' => PaperScissorsStone.Scissors
    case _ => throw new IllegalArgumentException(s"Invalid input: $s")

  def fromStringCol2Part2(c1: Char, c2: Char): (Outcome, PaperScissorsStone) = {
    val firstShape = fromStringCol1(c1)
    val outcome = c2 match {
      case 'X' => Outcome.Lose
      case 'Y' => Outcome.Draw
      case 'Z' => Outcome.Win
      case _ => throw new IllegalArgumentException(s"Invalid input: $c2")
    }

    (outcome, firstShape.findShapeForOutcome(outcome))
  }
}

object PaperScissorsStoneScoreCalculator {
  def parseInput(lines: Iterator[String]): List[(PaperScissorsStone, PaperScissorsStone)] = {
    lines.map(_.split(" ").toList).map {
      case List(p1, p2) => (fromStringCol1(p1.charAt(0)), fromStringCol2(p2.charAt(0)))
      case _ => throw new IllegalArgumentException("Invalid input")
    }.toList
  }

  def parseInputPart2(lines: Iterator[String]): List[(Outcome, PaperScissorsStone)] = {
    lines.map(_.split(" ").toList).map {
      case List(p1, p2) => PaperScissorsStoneMapper.fromStringCol2Part2(p1.charAt(0), p2.charAt(0))
      case _ => throw new IllegalArgumentException("Invalid input")
    }.toList
  }

  def calculateScore(input: List[(PaperScissorsStone, PaperScissorsStone)]): List[Int] =
    input.map(i => shapeScore(i._2) + outcomeScore(i))

  def calculateTotalScore(input: List[(PaperScissorsStone, PaperScissorsStone)]): Int =
    calculateScore(input).sum

  def calculateScorePart2(input: List[(Outcome, PaperScissorsStone)]): List[Int] =
    input.map(i => shapeScore(i._2) + outcomeScore(i._1))

  def calculateTotalScorePart2(input: List[(Outcome, PaperScissorsStone)]): Int =
    calculateScorePart2(input).sum

  private def shapeScore(shape: PaperScissorsStone): Int = shape match {
    case PaperScissorsStone.Stone => 1
    case PaperScissorsStone.Paper => 2
    case PaperScissorsStone.Scissors => 3
  }

  private def outcomeScore(input: (PaperScissorsStone, PaperScissorsStone)): Int = outcomeScore(input._2 vs input._1)

  private def outcomeScore(outcome: Outcome): Int = outcome match {
    case Outcome.Win => 6
    case Outcome.Lose => 0
    case Outcome.Draw => 3
  }

}
class PaperScissorsStoneScoreCalculator {

}
