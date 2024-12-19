package aoc
package aoc2024.day4

import common.Utils.loadData

import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.Accumulator
import scala.util.matching.Regex

enum CheckDirection(x: Int, y: Int) {
  private case HorizontalRight extends CheckDirection(1, 0)
  private case HorizontalLeft extends CheckDirection(-1, 0)
  private case VerticalDown extends CheckDirection(0, 1)
  private case VerticalUp extends CheckDirection(0, -1)
  private case DiagonalUpRight extends CheckDirection(1, -1)
  private case DiagonalUpLeft extends CheckDirection(-1, -1)
  private case DiagonalDownRight extends CheckDirection(1, 1)
  private case DiagonalDownLeft extends CheckDirection(-1, 1)

  def getX: Int = x

  def getY: Int = y

}

object WordSearcher {
  def parseInput(input: Iterator[String]): List[List[Char]] = input.map(line => line.toList).toList

  def countWordOccurrences(word: String, allChars: List[List[Char]]): Int = {
    var count = 0
    for (row <- allChars.indices)
      for (col <- allChars(row).indices)
        if (allChars(row)(col) == word.head)
          val singleWordOccurrences = countWordOccurrencesAtPosition(word.toList.tail, allChars, col, row)
          count = count + singleWordOccurrences
    count
  }

  private def countWordOccurrencesAtPosition(chars: List[Char], allChars: List[List[Char]], x: Int, y: Int): Int = {
    // x = col, y = row
    val occurrences = CheckDirection.values.map(direction => {
      val distance = chars.length
      val endX = x + distance * direction.getX
      val endY = y + distance * direction.getY
      val matchCount = if (endX >= 0 && endX <= allChars.head.length - 1 && endY >= 0 && endY <= allChars.length - 1)
        val charsOnPath = for (i <- 1 to distance) yield allChars(y + i * direction.getY)(x + i * direction.getX)
        if (charsOnPath == chars) 1 else 0
      else 0
//      println(s"$y $x $direction $endY $endX - $matchCount")
      matchCount
    }).sum
    occurrences
  }
  
  def countCrossWordOccurrences(word: String, allChars: List[List[Char]]): Int = {
    var count = 0
    for (row <- allChars.indices)
      for (col <- allChars(row).indices)
        if (allChars(row)(col) == 'A')
          val singleWordOccurrences = countCrossWordOccurrencesAtPosition(allChars, col, row)
          count = count + singleWordOccurrences
    count
  }
  
  private def countCrossWordOccurrencesAtPosition(allChars: List[List[Char]], x: Int, y: Int): Int = {
    // x = col, y = row
    val matchCount = if (x > 0 && x < allChars.head.length - 1 && y > 0 && y < allChars.length - 1)
//      println(s"$y $x")
      if ((allChars(y+1)(x+1) == 'S' && allChars(y-1)(x-1) == 'M' || allChars(y+1)(x+1) == 'M' && allChars(y-1)(x-1) == 'S') 
        && (allChars(y-1)(x+1) == 'S' && allChars(y+1)(x-1) == 'M' || allChars(y-1)(x+1) == 'M' && allChars(y+1)(x-1) == 'S'))
        1
      else 0
    else 0
    matchCount
  }

}

@main def main(): Unit = {

  val filename = "aoc2024-day4-input.txt"
//    val filename = "test.txt"
  val content = loadData(filename)(WordSearcher.parseInput)
  val word = "XMAS"
  val occurrences = WordSearcher.countWordOccurrences(word, content)
  println(s"The word $word occurred $occurrences times in the input.")
  
  val xWord = "MAS"
  val occurrences2 = WordSearcher.countCrossWordOccurrences(xWord, content)
  println(s"The x-word $xWord occurred $occurrences2 times in the input.")
}