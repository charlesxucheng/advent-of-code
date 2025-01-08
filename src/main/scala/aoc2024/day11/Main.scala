package aoc
package aoc2024.day11

import aoc2024.day11.Stones.blink
import common.NumberList
import common.Utils.loadData
import scala.annotation.tailrec
import scala.collection.MapView
import scala.collection.immutable.HashMap

object Stones {
  private def applyRule(number: BigInt): Seq[BigInt] = {
    if (number == 0) Seq(1)
    else if (hasEvenDigits(number)) splitIntoTwo(number)
    else Seq(number * 2024)
  }

  def blink(numbers: Seq[BigInt], times: Int): BigInt = {
    if (times <= 40) {
      val result = (1 to times).foldLeft(numbers)((seq, _) =>
        seq.flatMap(n => applyRule(n))
      )
      result.size
    } else {
      val numberMap =
        numbers.groupBy(identity).view.mapValues(c => BigInt(c.size))
      val resultMap = transformRec(numberMap, times)
      resultMap.values.sum
    }
  }

  @tailrec
  private def transformRec(
      numbers: MapView[BigInt, BigInt],
      times: Int
  ): MapView[BigInt, BigInt] = {
    if (times >= 66) {
      println(s"Iteration: ${75 - times}")
      numbers.foreach(pair => println(s"${pair._1} -> ${pair._2}"))
    }
    if (times == 0) numbers
    else {
      val newMap: MapView[BigInt, BigInt] =
        numbers
          .foldLeft(HashMap.empty[BigInt, BigInt]) { (map, entry) =>
            {
              val (number, count) = entry
              val newNumbers = applyRule(number).map((_, count))
              newNumbers.foldLeft(map)((map, pair) =>
                map
                  .updated(pair._1, map.getOrElse(pair._1, BigInt(0)) + pair._2)
              )
            }
          }
          .view
      transformRec(newMap, times - 1)
    }
  }

  private def hasEvenDigits(l: BigInt): Boolean =
    l.toString.toList.size % 2 == 0 && l != 0

  private def splitIntoTwo(l: BigInt): Seq[BigInt] = {
    require(hasEvenDigits(l))
    val digits = l.toString.toList
    val mid = digits.size / 2
    val (left, right) = digits.splitAt(mid)
    val leftNum = BigInt(left.mkString)
    val rightNum = BigInt(right.mkString)
    Seq(leftNum, rightNum)
  }

}

@main def main(): Unit = {

  val filename = "aoc2024-day11-input.txt"
//  val filename = "test.txt"
  val numbers: Seq[BigInt] =
    loadData(filename)(NumberList.parseInput(" ")).head.map(BigInt(_))
  println(numbers)

  println(s"Part 1 output: ${blink(numbers, 25)}")
  println(s"Part 2 output: ${blink(numbers, 75)}")

}
