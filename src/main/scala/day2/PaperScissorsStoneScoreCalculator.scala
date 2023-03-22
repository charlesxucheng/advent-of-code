package aoc2022
package day2

import scala.io.Source
import PaperScissorsStone._
def loadData(filename: String): List[(PaperScissorsStone, PaperScissorsStone)] = {
  val source = Source.fromFile(filename)
  try {
    PaperScissorsStoneScoreCalculator.parseInput(source.getLines())
  } finally source.close()
}

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
}

object PaperScissorsStoneScoreCalculator {
  def parseInput(lines: Iterator[String]): List[(PaperScissorsStone, PaperScissorsStone)] = {
    lines.map(_.split(" ").toList).map {
      case List(p1, p2) => (PaperScissorsStoneMapper.fromStringCol1(p1.charAt(0)), PaperScissorsStoneMapper.fromStringCol2(p2.charAt(0)))
      case _ => throw new IllegalArgumentException("Invalid input")
    }.toList
  }

  def calculateScore(input: List[(PaperScissorsStone, PaperScissorsStone)]): List[Int] =
    input.map(i => shapeScore(i._2) + outcomeScore(i))

  def calculateTotalScore(input: List[(PaperScissorsStone, PaperScissorsStone)]): Int =
    calculateScore(input).sum
    
  private def shapeScore(shape: PaperScissorsStone): Int = shape match {
    case PaperScissorsStone.Stone => 1
    case PaperScissorsStone.Paper => 2
    case PaperScissorsStone.Scissors => 3
  }

  private def outcomeScore(input: (PaperScissorsStone, PaperScissorsStone)): Int = input._2 vs input._1 match {
    case Outcome.Win => 6
    case Outcome.Lose => 0
    case Outcome.Draw => 3
  }

}
class PaperScissorsStoneScoreCalculator {

}
