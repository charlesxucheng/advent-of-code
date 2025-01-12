package aoc
package aoc2024.day5

import aoc2024.day5.RuleValidator.*
import common.NumberList
import common.Utils.loadData
import scala.annotation.tailrec

object RuleValidator {

  def parseRulesInput(input: Iterator[String]): List[Rule] =
    input
      .map(_.split("\\|").take(2))
      .map(pair => Rule(pair(0).toInt, pair(1).toInt))
      .toList

  extension (rules: List[Rule]) {

    def findValidPageUpdates(
        pages: List[List[Int]]
    ): List[List[Int]] =
      pages.filter(p => allRulesFollowed(p))

    def findInvalidPageUpdates(
        pages: List[List[Int]]
    ): List[List[Int]] =
      pages.filter(p => someRulesNotFollowed(p))

    def correctPageOrders(
        pageUpdates: List[List[Int]]
    ): List[List[Int]] = {
      pageUpdates.map { pages =>
        {
          updatePageOrder(pages, rules, rules)
        }
      }
    }

    private def someRulesNotFollowed(pages: List[Int]): Boolean = {
      rules.exists(rule => {
        val index1 = pages.indexOf(rule.first)
        val index2 = pages.indexOf(rule.second)
        index1 >= 0 && index2 >= 0 && index1 > index2
      })
    }

    private def allRulesFollowed(pages: List[Int]): Boolean =
      !someRulesNotFollowed(pages)

    @tailrec
    private def updatePageOrder(
        oneSetOfUpdates: List[Int],
        remainingRules: List[Rule],
        allRules: List[Rule]
    ): List[Int] = {
      remainingRules match {
        case Nil => oneSetOfUpdates
        case head :: tail =>
          val index1 = oneSetOfUpdates.indexOf(head.first)
          val index2 = oneSetOfUpdates.indexOf(head.second)
          if (index1 >= 0 && index2 >= 0 && index1 > index2)
            // If there is a swap of elements, recheck all rules as some already passed rules may be violated by the swap.
            updatePageOrder(
              swapElements(oneSetOfUpdates, index1, index2),
              allRules,
              allRules
            )
          else
            updatePageOrder(oneSetOfUpdates, tail, allRules)
      }
    }

    private def swapElements(list: List[Int], i: Int, j: Int) =
      list.updated(j, list(i)).updated(i, list(j))
  }

  def midEntries(validInputs: List[List[Int]]): List[Int] =
    validInputs.map(l => l(l.size / 2))

  def checkNoDuplicates(allPages: List[List[Int]]): Boolean =
    allPages
      .map(pages => {
        pages.groupBy(x => x).forall((k, v) => v.length == 1)
      })
      .forall(x => x)

}

case class Rule(first: Int, second: Int)

@main def main(): Unit = {

  val rulesFileName = "aoc2024-day5-input3.txt"
  val dataFileName = "aoc2024-day5-input4.txt"
  val rules = loadData(rulesFileName)(parseRulesInput)
  println(rules.size)
  val data = loadData(dataFileName)(NumberList.parseInput(","))
  println(data.size)

  println(
    s"No list of updated pages contain duplicates: ${checkNoDuplicates(data)}"
  )

  val validInputs = rules.findValidPageUpdates(data)
  println(validInputs)

  val mid = midEntries(validInputs)
  println(mid)
  println(s"Part 1: Sum of mid entries: ${mid.sum}")

  val invalidInputs = rules.findInvalidPageUpdates(data)
  println(invalidInputs)

  val correctedInvalidInputs = rules.correctPageOrders(invalidInputs)
  val mid2 = midEntries(correctedInvalidInputs)
  println(mid2)
  println(s"Part 2: Sum of mid entries: ${mid2.sum}")

}
