package aoc
package aoc2024.day2

import aoc2024.day2.Trend.{Ascending, Descending, Equal}

import aoc2022.common.Utils.loadData
import jdk.jfr.Threshold

import scala.annotation.tailrec
import scala.io.Source

object NumberList {
  def parseInput(lines: Iterator[String]): List[List[Int]] =
    lines
      .map(_.split(" ")
        .map(_.toInt)
        .toList)
      .toList
}

enum Trend {
  case Ascending
  case Descending
  case Equal
}

case class Report(numbers: List[Int]) {
  require(numbers.size > 4)

  val increasingDiffCount: Int = numbers.zip(numbers.tail).count(_ < _)
  val dominantTrend: Trend = if (increasingDiffCount > (numbers.size / 2)) Ascending else Descending

  override def toString: String = numbers.toString()

  def isSafe(tolerance: Int): Boolean =
    if (tolerance == 0)
      isSafeRec(numbers.head, numbers.tail, tolerance)
    else
      isSafeRec(numbers.head, numbers.tail, tolerance) || isSafeRec(numbers.tail.head, numbers.tail.tail, tolerance - 1)

  @tailrec
  private def isSafeRec(head: Int, tail: List[Int], tolerance: Int): Boolean = {

//    println(s"Entering isSafeRec: $prev $remainingNumbers $trend $tolerance")

    tail match {
      case Nil => true
      case x :: xs =>
        val trendOfPair = if (head < x) Ascending else if (head > x) Descending else Equal
        if (trendOfPair == dominantTrend)
          trendOfPair match {
            case Ascending =>
              if (x - head >= 1 && x - head <= 3)
                isSafeRec(x, xs, tolerance)
              else if (tolerance > 0)
                isSafeRec(head, xs, tolerance - 1)
              else
                false
            case Descending =>
              if (x - head >= -3 && x - head <= -1)
                isSafeRec(x, xs,  tolerance)
              else if (tolerance > 0)
                isSafeRec(head, xs, tolerance - 1)
              else
                false
            case Equal =>
              throw IllegalStateException("Not possible")
          }
        else {
          if (tolerance > 0) isSafeRec(head, xs, tolerance - 1)
          else false
        }
    }
  }
}

@main def main(): Unit = {

  val filename = "aoc2024-day2-input.txt"
  val reports = loadData(filename)(NumberList.parseInput)
  reports.foreach(x => {
    val report = Report(x)
    val safe0 = report.isSafe(0)
    val safe1 = report.isSafe(1)
    if (safe0 == safe1)
      println(s"$report | ${report.increasingDiffCount} | ${report.dominantTrend} | Safe(0): $safe0 | Safe(1): $safe1")
  })

  val safeReportsWithoutTolerance = reports.filter(Report(_).isSafe(0))
  val safeReportsWithTolerance = reports.filter(Report(_).isSafe(1))

  println(s"There are ${safeReportsWithoutTolerance.size} safe reports")
  println(s"There are ${safeReportsWithTolerance.size} safe reports")

}