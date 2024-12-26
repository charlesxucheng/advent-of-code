package aoc
package aoc2024.day7

import aoc2024.day7.Calculator.{allPossibleResults, parseInput}
import common.Utils.loadData

import scala.annotation.tailrec

case class CalculationComponents(result: Long, operands: List[Long])

object Calculator {
  def parseInput(input: Iterator[String]): List[CalculationComponents] =
    input.map(_.split(":"))
      .map { components =>
        val result = components(0).toLong
        val operands = components(1).trim.split(" ").map(_.toLong).toList
        CalculationComponents(result, operands)
      }.toList

  @tailrec
  def allPossibleResults(operands: List[Long], accumulatedResults: Set[Long]): Set[Long] =
    operands match {
      case Nil => accumulatedResults
      case head :: tail =>
        if (accumulatedResults.isEmpty)
          allPossibleResults(operands.tail, Set(operands.head))
        else 
          allPossibleResults(
            operands.tail,
            accumulatedResults.flatMap { r =>
              Set(r + operands.head, r * operands.head, (r.toString + operands.head.toString).toLong)
            }
          )
    }
}

@main def main(): Unit = {

  val filename = "aoc2024-day7-input.txt"
  //  val filename = "test.txt"
  val calculationComponents = loadData(filename)(parseInput)

  val matchingResults = calculationComponents.flatMap { components =>
    val possibleResults = allPossibleResults(components.operands, Set.empty)
    possibleResults.filter(_ == components.result)
  }
  println(matchingResults)
  println(s"Total is ${matchingResults.sum}")

}