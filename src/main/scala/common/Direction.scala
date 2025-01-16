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

  def turnLeft: Direction = this match {
    case North => West
    case West  => South
    case South => East
    case East  => North
  }
}
