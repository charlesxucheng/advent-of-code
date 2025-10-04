package aoc
package aco2022.day8

import aco2022.day8.TreeHouse.{findVisibility, getBorderSize}
import common.TwoDMap
import common.TwoDimensionalArray.print2DArray
import common.Utils.loadData

object TreeHouse {

  def findVisibility(trees: Array[Array[Int]]): Array[Array[Boolean]] = {

    val leftMaxHeights = dropFirstAndLast(trees.map(row => row(0)))
    val rightMaxHeights =
      dropFirstAndLast(trees.map(row => row(row.length - 1)))
    val topMaxHeights = dropFirstAndLast(trees(0))
    val bottomMaxHeights = dropFirstAndLast(trees(trees.length - 1))

    val resultLeft =
      findVisibilityInDirection(trees, leftMaxHeights, Direction.Left)
    println("Result from left:")
    print2DArray(resultLeft)

    val resultRight =
      findVisibilityInDirection(trees, rightMaxHeights, Direction.Right)
    println("Result from right:")
    print2DArray(resultRight)

    val resultTop =
      findVisibilityInDirection(trees, topMaxHeights, Direction.Top)
    println("Result from top:")
    print2DArray(resultTop)

    val resultBottom =
      findVisibilityInDirection(trees, bottomMaxHeights, Direction.Bottom)
    println("Result from bottom:")
    print2DArray(resultBottom)

    Array.tabulate(sizeY(trees), sizeX(trees)) { (y, x) =>
      resultLeft(y)(x)
      || resultRight(y)(x)
      || resultTop(y)(x)
      || resultBottom(y)(x)
    }
  }

  private def findVisibilityInDirection(
      trees: Array[Array[Int]],
      maxHeights: Array[Int],
      direction: Direction
  ): Array[Array[Boolean]] = {
    val result = initializeResult(trees)

    val (outerRange, innerRange, isRowMajor) = direction match {
      case Direction.Left =>
        (1 until sizeY(trees) - 1, 1 until sizeX(trees) - 1, true)
      case Direction.Right =>
        (1 until sizeY(trees) - 1, sizeX(trees) - 2 to 1 by -1, true)
      case Direction.Top =>
        (1 until sizeX(trees) - 1, 1 until sizeY(trees) - 1, false)
      case Direction.Bottom =>
        (1 until sizeX(trees) - 1, sizeY(trees) - 2 to 1 by -1, false)
    }

    for {
      outer <- outerRange
      inner <- innerRange
    } {
      val (y, x) = if (isRowMajor) (outer, inner) else (inner, outer)
      val maxHeightIndex = outer - 1
      val treeHeight = trees(y)(x)
      val maxHeight = maxHeights(maxHeightIndex)
      if (treeHeight > maxHeight) {
        maxHeights(maxHeightIndex) = treeHeight
        result(y)(x) = true
      }
    }
    result
  }

  private def initializeResult(trees: Array[Array[Int]]) = {
    Array.fill[Boolean](sizeY(trees), sizeX(trees))(false)
  }

  private def dropFirstAndLast(heights: Array[Int]): Array[Int] =
    heights.drop(1).dropRight(1)

  private def sizeY(trees: Array[Array[Int]]): Int = trees.length

  private def sizeX(trees: Array[Array[Int]]): Int = trees(0).length

  def getBorderSize(trees: Array[Array[Int]]): Int =
    sizeY(trees) * 2 + sizeX(trees) * 2 - 4

  private enum Direction {
    case Left, Right, Top, Bottom
  }
}

@main def main(): Unit = {
//  val fileName = "test.txt"
  val fileName = "aoc2022-day8-input.txt"
  val trees = loadData(fileName)(TwoDMap.parseInput(_.asDigit)).map

  val count = findVisibility(trees).map(_.count(_ == true)).sum
  println(s"Part 1 result: ${count + getBorderSize(trees)}")
}
