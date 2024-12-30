package aoc
package aoc2024.day8

import aoc2024.day8.AntennaMap.{calculateAntiNodes, parseInput}
import common.Position
import common.Utils.loadData

type AntennaMap = Map[Char, Set[(Position, Position)]]

object AntennaMap {
  private var rowSize: Int = 0
  private var colSize: Int = 0

  def parseInput(lines: Iterator[String]): AntennaMap = {
    val chars = common.CharList.parseInput(lines)
    colSize = chars.head.length
    rowSize = chars.length

    val charPositionsMap = chars.zipWithIndex
      .flatMap((line, rowIndex) => line.zipWithIndex.map((char, colIndex) => (char, Position(colIndex, rowIndex))))
      .filter(_._1 != '.')
      .groupMap(_._1)(_._2)

    charPositionsMap.map(entry => {
      val valuePairs = entry._2.combinations(2).map(pair => (pair.head, pair.last)).toSet
      entry._1 -> valuePairs
    })
  }

  def calculateAntiNodes(antennaPair: (Position, Position)): Set[Position] = {
    val (a, b) = antennaPair
    println(s"a = $a, b = $b")

    if (a == b) Set.empty
    else {
      val xDistance = Math.abs(a.x - b.x)
      val yDistance = Math.abs(a.y - b.y)
//      println(s"xDistance $xDistance yDistance $yDistance")
      val antiNodes: Set[Position] =
        if (a.x >= b.x && a.y >= b.y)
          Set(Position(a.x + xDistance, a.y + yDistance), Position(b.x - xDistance, b.y - yDistance))
        else if (a.x <= b.x && a.y <= b.y) {
          Set(Position(a.x - xDistance, a.y - yDistance), Position(b.x + xDistance, b.y + yDistance))
        } else if (a.x >= b.x && a.y <= b.y)
          Set(Position(a.x + xDistance, a.y - yDistance), Position(b.x - xDistance, b.y + yDistance))
        else if (a.x <= b.x && a.y >= b.y)
          Set(Position(a.x - xDistance, a.y + yDistance), Position(b.x + xDistance, b.y - yDistance))
        else Set.empty

      val result: Set[Position] = antiNodes.filter(node => node.x >= 0 && node.y >= 0 && node.x < colSize && node.y < rowSize)
      println(s"Anti nodes: $result")
      result
    }
  }
}

@main def main(): Unit = {

  val filename = "aoc2024-day8-input.txt"
  //  val filename = "test.txt"
  val antennaMap = loadData(filename)(parseInput)
  println(antennaMap)
  val antiNodes = antennaMap.flatMap(_._2.flatMap(calculateAntiNodes)).toSet
  println(s"Anti nodes size: ${antiNodes.size}")

}