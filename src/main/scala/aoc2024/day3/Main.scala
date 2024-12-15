package aoc
package aoc2024.day3

import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.Accumulator
import scala.util.matching.Regex

@main def main(): Unit = {

  val enablePattern = "do()"
  val disablePattern = "don't()"

  @tailrec
  def collectPartsRec(input: String, accumulated: String, enabled: Boolean): String = {
    if (enabled) {
      val index = input.indexOf(disablePattern)
      if (index == -1) accumulated
      else collectPartsRec(input.drop(index + disablePattern.length), accumulated + input.take(index), false)
    } else {
      val index = input.indexOf(enablePattern)
      if (index == -1) accumulated
      else collectPartsRec(input.drop(index + enablePattern.length), accumulated, true)
    }
  }

  val filename = "aoc2024-day3-input.txt"
  val source = Source.fromFile(filename)

  def sumOfProducts(content: String) = {

    val pattern = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r

    val total = pattern.findAllMatchIn(content)
      .map(matched => (matched.group(1).toInt, matched.group(2).toInt))
      .map(pair => pair._1 * pair._2)
      .sum
    total
  }

  try {
    val content = source.getLines().reduceLeft(_ + _)

    val filteredContent = collectPartsRec(content, "", true)

    val total: Int = sumOfProducts(content)
    val total2: Int = sumOfProducts(filteredContent)

    println(s"Total: $total")
    println(s"Total with filtered content: $total2")

  }
  catch {
    case e: Exception => println(e)
  }
  finally {
    source.close()
  }

}