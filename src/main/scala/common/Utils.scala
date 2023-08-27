package aoc2022
package common

import scala.io.Source

object Utils {

  def loadData[T](filename: String)(parser: Iterator[String] => List[T]): List[T] = {
    val source = Source.fromFile(filename)
    try {
      parser(source.getLines())
    } finally source.close()
  }
}
