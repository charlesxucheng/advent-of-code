package aoc
package common

import scala.io.Source

object Utils {

  def loadData[T, C[_]](filename: String)(parser: Iterator[String] => C[T]): C[T] = {
    val source = Source.fromFile(filename)
    try {
      parser(source.getLines())
    } finally source.close()
  }
}

object NumberList {
  def parseInput(delimiter: String)(lines: Iterator[String]): List[List[Int]] =
    lines
      .map(_.split(delimiter)
        .map(_.toInt)
        .toList)
      .toList
}

object CharList {
  def parseInput(input: Iterator[String]): List[List[Char]] = input.map(line => line.toList).toList
}
