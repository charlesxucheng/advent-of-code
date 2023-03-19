package aoc2022
package Day1


import scala.io.Source

@main def main(): Unit = {

  val filename = "day1-input.txt"
  val source = Source.fromFile(filename)
  val calorieList = try {
    CalorieCalculator.parseInput(source.getLines())
  } finally source.close()
  println(s"Answer is: ${calorieList.max}")
}


object CalorieCalculator {

  def parseInput(input: String): List[Int] = {
    if (input == null || input.isEmpty)
      List.empty
    else
      val list = input.split(";;").toList
        .map(_.split(";"))
        .map(_.map(_.toInt).toList)
      list.map(_.sum)
  }
  def parseInput(input: Iterator[String]): List[Int] = {
    if (input == null || input.isEmpty)
      List.empty
    else
      val inputStr = input.map(_.trim).mkString(";")
      parseInput(inputStr)
  }
}
class CalorieCalculator() {
}