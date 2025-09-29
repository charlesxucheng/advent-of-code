package aoc
package common

import scala.io.Source
import scala.reflect.ClassTag

object Utils {

  def loadData[T, C[_]](
      filename: String
  )(parser: Iterable[String] => C[T]): C[T] = {
    val source = Source.fromFile(filename)
    try {
      parser(source.getLines().to(LazyList))
    } finally source.close()
  }
 
}

object NumberList {
  def parseInput(delimiter: String)(lines: Iterable[String]): List[List[Int]] =
    lines
      .map(
        _.split(delimiter)
          .map(_.toInt)
          .toList
      )
      .toList
}

object CharList {
  def parseInput(input: Iterable[String]): List[List[Char]] =
    input.map(line => line.toList).toList
}

object TwoDimensionalArray {
  def parseInput[T: ClassTag](converter: Char => T)(
      input: Iterator[String]
  ): Array[Array[T]] =
    input.map(line => line.toArray.map(converter)).toArray
}
