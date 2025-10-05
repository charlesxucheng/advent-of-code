package aoc
package aco2022.day8

import aco2022.day8.Trees.*
import common.TwoDMap
import common.TwoDimensionalArray.print2DArray
import common.Utils.loadData

object Trees {

  private def initializeResult(trees: Trees) = {
    Array.fill[Boolean](trees.sizeY, trees.sizeX)(false)
  }

  private def dropFirstAndLast(heights: Array[Int]): Array[Int] =
    heights.drop(1).dropRight(1)

  private def sizeX(trees: Array[Array[Int]]): Int = trees(0).length

  private enum Direction {
    case Left, Right, Top, Bottom
  }
}

case class Trees(trees: Array[Array[Int]]) {
  require(sizeX > 0 && sizeY > 0)

  def getBorderSize: Int = sizeY * 2 + sizeX * 2 - 4

  def maxScenicScore(): Int = {
    var max = 0
    for {
      y <- 0 until sizeY
      x <- 0 until sizeX
    } {
      val score = scenicScore(x, y)
      if (score > max) max = score
    }
    max
  }

  private def scenicScore(x: Int, y: Int): Int = {
    val left = viewingDistance(x, y, -1, 0)
    val right = viewingDistance(x, y, 1, 0)
    val top = viewingDistance(x, y, 0, -1)
    val bottom = viewingDistance(x, y, 0, 1)
    left * right * top * bottom
  }

  private def viewingDistance(
      x: Int,
      y: Int,
      offsetX: Int,
      offsetY: Int
  ): Int = {
    var distance = 0
    val current = trees(y)(x)
    var next = -1
    var newX = x + offsetX
    var newY = y + offsetY
    while (isWithin(newX, newY) && next < current) {
      distance += 1
      next = trees(newY)(newX)
      newX += offsetX
      newY += offsetY
    }
    distance
  }

  private def isWithin(x: Int, y: Int) =
    x >= 0 && x < sizeX && y >= 0 && y < sizeY

  def findVisibility(): Array[Array[Boolean]] = {

    val leftMaxHeights = Trees.dropFirstAndLast(trees.map(row => row(0)))
    val rightMaxHeights =
      dropFirstAndLast(trees.map(row => row(row.length - 1)))
    val topMaxHeights = dropFirstAndLast(trees(0))
    val bottomMaxHeights = dropFirstAndLast(trees(trees.length - 1))

    val resultLeft =
      findVisibilityInDirection(leftMaxHeights, Direction.Left)
    println("Result from left:")
    print2DArray(resultLeft)

    val resultRight =
      findVisibilityInDirection(rightMaxHeights, Direction.Right)
    println("Result from right:")
    print2DArray(resultRight)

    val resultTop =
      findVisibilityInDirection(topMaxHeights, Direction.Top)
    println("Result from top:")
    print2DArray(resultTop)

    val resultBottom =
      findVisibilityInDirection(bottomMaxHeights, Direction.Bottom)
    println("Result from bottom:")
    print2DArray(resultBottom)

    Array.tabulate(sizeY, sizeX) { (y, x) =>
      resultLeft(y)(x)
      || resultRight(y)(x)
      || resultTop(y)(x)
      || resultBottom(y)(x)
    }
  }

  def sizeX: Int = trees(0).length
  def sizeY: Int = trees.length

  private def findVisibilityInDirection(
      maxHeights: Array[Int],
      direction: Direction
  ): Array[Array[Boolean]] = {
    val result = initializeResult(this)

    val (outerRange, innerRange, isRowMajor) = direction match {
      case Direction.Left =>
        (1 until sizeY - 1, 1 until sizeX - 1, true)
      case Direction.Right =>
        (1 until sizeY - 1, sizeX - 2 to 1 by -1, true)
      case Direction.Top =>
        (1 until sizeX - 1, 1 until sizeY - 1, false)
      case Direction.Bottom =>
        (1 until sizeX - 1, sizeY - 2 to 1 by -1, false)
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

}

@main def main(): Unit = {
//  val fileName = "test.txt"
  val fileName = "aoc2022-day8-input.txt"
  val trees = Trees(loadData(fileName)(TwoDMap.parseInput(_.asDigit)).map)

  val count = trees.findVisibility().map(_.count(_ == true)).sum
  println(s"Part 1 result: ${count + trees.getBorderSize}")

  val score = trees.maxScenicScore()
  println(s"Part 2 result: $score")
}
