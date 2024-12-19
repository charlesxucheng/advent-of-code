package aoc
package aoc2024.day5

import aoc2024.day5.RuleValidator.{checkNoDuplicates, findValidUpdates, parseRulesInput, midEntries}

import common.NumberList
import common.Utils.loadData

object RuleValidator {

  def parseRulesInput(input: Iterator[String]): List[Rule] =
    input.map(_.split("\\|").take(2))
      .map(pair => Rule(pair(0).toInt, pair(1).toInt))
      .toList

  def findValidUpdates(rules: List[Rule], pages: List[List[Int]]): List[List[Int]] = {
    val a = pages.filter(p => {
      !rules.exists(rule => {
        val index1 = p.indexOf(rule.first)
        val index2 = p.indexOf(rule.second)
        index1 >= 0 && index2 >= 0 && index1 > index2
      })
    })
    a
  }

  def midEntries(validInputs: List[List[Int]]): List[Int] = validInputs.map(l => l(l.size / 2))

  def checkNoDuplicates(allPages: List[List[Int]]): Boolean =
    allPages.map(pages => {
      pages.groupBy(x => x).forall((k, v) => v.length == 1)
    }).forall(x => x)
}

case class Rule(first: Int, second: Int)

@main def main(): Unit = {

  val rulesFileName = "aoc2024-day5-input3.txt"
  val dataFileName = "aoc2024-day5-input4.txt"
  val rules = loadData(rulesFileName)(parseRulesInput)
  println(rules.size)
  val data = loadData(dataFileName)(NumberList.parseInput(","))
  println(data.size)

  println(s"No list of updated pages contain duplicates: ${checkNoDuplicates(data)}")

  val validInputs = findValidUpdates(rules, data)
  println(validInputs)

  val mid = midEntries(validInputs)
  println(mid)
  println(s"Sum of mid entries: ${mid.sum}")
}
