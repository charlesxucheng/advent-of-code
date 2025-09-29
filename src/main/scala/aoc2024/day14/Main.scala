package aoc
package aoc2024.day14

import common.Position
import common.Utils.loadData
import scala.annotation.tailrec

case class Velocity(x: Int, y: Int)

case class Map(width: Int, height: Int) {
  val minXIndex: Int = 0
  val minYIndex: Int = 0
  val maxXIndex: Int = width - 1
  val maxYIndex: Int = height - 1
}

case class Robot(position: Position, velocity: Velocity, map: Map) {
  def move(seconds: Int = 1): Robot =
    this.copy(position =
      teleport(
        position.x + velocity.x * seconds,
        position.y + velocity.y * seconds
      )
    )

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

  @tailrec
  private def teleport(x: Int, y: Int): Position = {
    val newX =
      if (x < map.minXIndex) x + map.width
      else if (x > map.maxXIndex) x - map.width
      else x
    val newY =
      if (y < map.minYIndex) y + map.height
      else if (y > map.maxYIndex) y - map.height
      else y
    if ((newX, newY) == (x, y))
      Position(newX, newY)
    else teleport(newX, newY)
  }
}

object Robot {
  def parseInput(input: Iterable[String]): Seq[Robot] = {
    val map = Map(width = 101, height = 103)
    val robots = input.map:
      case s"p=$px,$py v=$vx,$vy" =>
        Robot(Position(px.toInt, py.toInt), Velocity(vx.toInt, vy.toInt), map)

    robots.toSeq
  }

  def detectChristmasTree(robots: Seq[Robot]): Boolean = {
    //
    val a = findVerticalLineCount(15, robots)
    val b = findHorizontalLineCount(12, robots)

    a >= 3 && b >= 12
  }

  private def findVerticalLineCount(threshold: Int, robots: Seq[Robot]): Int = {
    robots.groupBy(_.position.x).count(_._2.size >= threshold)
  }

  private def findHorizontalLineCount(threshold: Int, robots: Seq[Robot]): Int =
    robots.groupBy(_.position.y).count(_._2.size >= threshold)

}

@main def main(): Unit = {

  val filename = "aoc2024-day14-input.txt"
//  val filename = "test.txt"

  val robots = loadData(filename)(Robot.parseInput)

  val results = robots.map(_.move(100))

  println(results)

  val safetyFactors = results
    .flatMap(_.inQuadrantOfMap)
    .groupBy(identity)
    .toSeq
    .map((k, v) => v.size)

  println(s"Safety factor = ${safetyFactors.product}")

  // Part 2
  for i <- 1 to 100000 do
    val results = robots.map(_.move(i))
    if i % 1000 == 0 then println(s"Step $i")
    if Robot.detectChristmasTree(results) then
      println(s"Christmas tree detected at step $i")
      System.exit(0)

}
