package aoc
package common

enum Direction {
  case North, East, South, West

  def turnRight: Direction = this match {
    case North => East
    case East  => South
    case South => West
    case West  => North
  }
}
