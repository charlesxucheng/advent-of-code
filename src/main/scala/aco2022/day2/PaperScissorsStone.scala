package aoc
package aco2022.day2

enum Outcome {
  case Win, Lose, Draw
}
enum PaperScissorsStone {
  case Paper, Scissors, Stone

  private infix def beats(that: PaperScissorsStone): Boolean = this match
    case Paper => that == Stone
    case Scissors => that == Paper
    case Stone => that == Scissors

  private infix def ties(that: PaperScissorsStone): Boolean = this == that

  infix def vs(that: PaperScissorsStone): Outcome = {
    if this beats that then Outcome.Win
    else if this ties that then Outcome.Draw
    else Outcome.Lose
  }

  import Outcome.*

  // Returns the shape that would produce the given outcome when the first shape is this one
  def findShapeForOutcome(Outcome: Outcome): PaperScissorsStone = Outcome match
    case Win => this match
      case Paper => Scissors
      case Scissors => Stone
      case Stone => Paper
    case Lose => this match
      case Paper => Stone
      case Scissors => Paper
      case Stone => Scissors
    case Draw => this
}
