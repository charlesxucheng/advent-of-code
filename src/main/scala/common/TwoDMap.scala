package aoc
package common

import scala.reflect.ClassTag

case class TwoDMap[T: ClassTag](map: Array[Array[T]]) {

  def display(): Unit =
    map.foreach(row => println(row.mkString("")))

  def findFirst(cellContent: T): Option[(Int, Int)] = {
    for {
      row <- map.indices
      col <- map(row).indices
      if map(row)(col) == cellContent
    } yield (row, col)
  }.headOption

  def findAll(value: T): Set[Position] =
    (for {
      (row, rowIndex) <- map.zipWithIndex
      (column, columnIndex) <- row.zipWithIndex
      if map(rowIndex)(columnIndex) == value
    } yield Position(columnIndex, rowIndex)).toSet

  def hasValue(position: Position, value: T): Boolean =
    map(position.y)(position.x) == value

  def isOnBorder(
      position: Position
  ): Boolean =
    position.x == 0 || position.x == map.head.length - 1 || position.y == 0 || position.y == map.length - 1

  def contains(p: Position): Boolean =
    p.x >= 0 && p.y >= 0 && p.x < map.head.length && p.y < map.length

  def get(position: Position): T =
    map(position.y)(position.x)

  def getColRange: Range = map.head.indices
  def getRowRange: Range = map.indices

  def updated(position: Position, value: T): TwoDMap[T] =
    TwoDMap(map.updated(position.y, map(position.y).updated(position.x, value)))

  def allPositions: Seq[Position] = {
    for {
      row <- getRowRange
      col <- getColRange
    } yield Position(col, row)
  }
   
  def getAdjacentPositions(
      position: Position
  ): Set[Position] =
    position.cardinalPositions.filter(contains)
}

object TwoDMap {
  def parseInput[T: ClassTag](converter: Char => T)(
      input: Iterator[String]
  ): TwoDMap[T] =
    TwoDMap(TwoDimensionalArray.parseInput(converter)(input))
}
