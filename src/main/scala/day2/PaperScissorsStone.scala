package aoc2022
package day2

enum Outcome {
  case Win, Lose, Draw
}
enum PaperScissorsStone {
  case Paper, Scissors, Stone

  private def beats(that: PaperScissorsStone): Boolean = this match
    case Paper => that == Stone
    case Scissors => that == Paper
    case Stone => that == Scissors

  private def ties(that: PaperScissorsStone): Boolean = this == that

  def vs(that: PaperScissorsStone): Outcome = {
    if this beats that then Outcome.Win
    else if this ties that then Outcome.Draw
    else Outcome.Lose
  }
}
