package aoc
package aco2022.day9

import aco2022.day8.Trees.*
import common.Utils.loadData
import common.{MovementDirection, Position}

import scala.annotation.tailrec

object RopeBridge {

  def parseInput(input: Iterable[String]): Iterable[Input] = {
    input.map(_.split(" ")).map { case Array(dir, steps) =>
      Input(
        dir match {
          case "U" => MovementDirection.Up
          case "D" => MovementDirection.Down
          case "L" => MovementDirection.Left
          case "R" => MovementDirection.Right
          case _ =>
            throw new IllegalArgumentException(s"Invalid direction: $dir")
        },
        steps.toInt
      )
    }.toList
  }

  def moveRopeWith(inputs: Iterable[Input]): Set[Position] = {
    @tailrec
    def moveRopeRec(
        rope: Rope,
        inputs: Iterable[Input],
        visited: Set[Position]
    ): Set[Position] = {
      inputs match {
        case Nil => visited
        case input :: rest =>
          val newRope = rope.move(input.direction)
          val remainingSteps = input.steps - 1
          if (remainingSteps > 0)
            moveRopeRec(
              newRope,
              Input(input.direction, remainingSteps) :: rest,
              visited + newRope.tail
            )
          else
            moveRopeRec(newRope, rest, visited + newRope.tail)
      }
    }
    moveRopeRec(Rope(Position(0, 0), Position(0, 0)), inputs, Set.empty)
  }

  case class Rope(head: Position, tail: Position) {
    require(touching(head, tail))

    def move(direction: MovementDirection): Rope = {
      val newHeadPosition = head.shift(direction)
      if (touching(newHeadPosition, tail))
        Rope(newHeadPosition, tail)
      else
        Rope(newHeadPosition, moveTail(direction))
    }

    private def moveTail(direction: MovementDirection): Position =
      if (head == tail) tail
      else if (head.x == tail.x || head.y == tail.y) tail.shift(direction)
      else head

    private def touching(head: Position, tail: Position) =
      (head.x - tail.x).abs < 2 && (head.y - tail.y).abs < 2

    private def touching: Boolean = touching(head, tail)
  }

  case class Input(direction: MovementDirection, steps: Int)
}

@main def main(): Unit = {
//  val fileName = "test.txt"
  val fileName = "aoc2022-day9-input.txt"

  val inputs = loadData(fileName)(RopeBridge.parseInput)
  val result = RopeBridge.moveRopeWith(inputs)

  println(s"Part 1 result: ${result.size}")
  println(s"Part 2 result: ")
}
