package aoc
package common

import scala.io.Source
import scala.reflect.ClassTag

object Utils {

  def loadData[T, C[_]](
      filename: String
  )(parser: Iterator[String] => C[T]): C[T] = {
    val source = Source.fromFile(filename)
    try {
      parser(source.getLines())
    } finally source.close()
  }
}

object NumberList {
  def parseInput(delimiter: String)(lines: Iterator[String]): List[List[Int]] =
    lines
      .map(
        _.split(delimiter)
          .map(_.toInt)
          .toList
      )
      .toList
}

object CharList {
  def parseInput(input: Iterator[String]): List[List[Char]] =
    input.map(line => line.toList).toList
}

object TwoDimensionalArray {
  def parseInput[T: ClassTag](converter: Char => T)(input: Iterator[String]): Array[Array[T]] =
    input.map(line => line.toArray.map(converter)).toArray
    
  def display[T: ClassTag](input: Array[Array[T]]): Unit = input.foreach(row => println(row.mkString("")))
}
