package aoc
package aoc2024.day14

import common.Position
import common.Utils.loadData

case class Velocity(x: Int, y: Int)

case class Map(width: Int, height: Int) {
  val minXIndex: Int = 0
  val minYIndex: Int = 0
  val maxXIndex: Int = width - 1
  val maxYIndex: Int = height - 1
}

case class Robot(position: Position, velocity: Velocity, map: Map) {
  def move(): Robot =
    this.copy(position =
      teleport(position.x + velocity.x, position.y + velocity.y)
    )

  private def teleport(x: Int, y: Int): Position = {
    val newX =
      if (x < map.minXIndex) x + map.width
      else if (x > map.maxXIndex) x - map.width
      else x
    val newY =
      if (y < map.minYIndex) y + map.height
      else if (y > map.maxYIndex) y - map.height
      else y
    Position(newX, newY)
  }

  def inQuadrantOfMap: Option[Int] = {
    val midXIndex = map.width / 2
    val midYIndex = map.height / 2
    val x = position.x - midXIndex
    val y = position.y - midYIndex
    if (x > 0 && y > 0) Some(1)
    else if (x < 0 && y > 0) Some(2)
    else if (x > 0 && y < 0) Some(3)
    else if (x < 0 && y < 0) Some(4)
    else None
  }
}

object Robot {
  def parseInput(input: Iterator[String]): Seq[Robot] = {
    val map = Map(width = 101, height = 103)
    val robots = input.map:
      case s"p=$px,$py v=$vx,$vy" =>
        Robot(Position(px.toInt, py.toInt), Velocity(vx.toInt, vy.toInt), map)

    robots.toSeq
  }

}

@main def main(): Unit = {

  val filename = "aoc2024-day14-input.txt"
//  val filename = "test.txt"

  val robots = loadData(filename)(Robot.parseInput)

  val results = (1 to 100).foldLeft(robots)((robots: Seq[Robot], i: Int) =>
    robots.map(_.move())
  )

//  println(results)

  val safetyFactors = results
    .flatMap(_.inQuadrantOfMap)
    .groupBy(identity)
    .toSeq
    .map((k, v) => v.size)

  println(s"Safety factor = ${safetyFactors.product}")

}
