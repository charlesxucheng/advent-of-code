package aoc
package aoc2024.day8

import aoc2024.day8.AntennaMap.{
  calculateAntiNodes,
  derivePart1AntiNodePositions,
  derivePart2AntiNodePositions,
  loadData
}
import common.Position

import scala.annotation.tailrec
import scala.io.Source

case class AntennaMap(
    rowSize: Int,
    colSize: Int,
    charPositionsMap: Map[Char, List[Position]],
    antennaPairs: Map[Char, Set[(Position, Position)]]
)

object AntennaMap {

  def loadData(filename: String): AntennaMap = {
    val source = Source.fromFile(filename)
    try {
      parseInput(source.getLines())
    } finally source.close()
  }

  def parseInput(lines: Iterator[String]): AntennaMap = {
    val chars = common.CharList.parseInput(lines.to(LazyList))
    val colSize = chars.head.length
    val rowSize = chars.length

    val charPositionsMap = chars.zipWithIndex
      .flatMap((line, rowIndex) =>
        line.zipWithIndex.map((char, colIndex) =>
          (char, Position(colIndex, rowIndex))
        )
      )
      .filter(_._1 != '.')
      .groupMap(_._1)(_._2)

    val antennaPairs = charPositionsMap.map(entry => {
      val valuePairs =
        entry._2.combinations(2).map(pair => (pair.head, pair.last)).toSet
      entry._1 -> valuePairs
    })

    AntennaMap(rowSize, colSize, charPositionsMap, antennaPairs)
  }

  def calculateAntiNodes(
      antennaPair: (Position, Position),
      colSize: Int,
      rowSize: Int,
      derivationFunction: (
          Position,
          Position,
          Int,
          Int,
          Int,
          Int,
          ((Int, Int), (Int, Int))
      ) => Set[Position]
  ): Set[Position] = {
    val (a, b) = antennaPair
    println(s"a = $a, b = $b")

    if (a == b) Set.empty
    else {
      val xDistance = Math.abs(a.x - b.x)
      val yDistance = Math.abs(a.y - b.y)
      //      println(s"xDistance $xDistance yDistance $yDistance")
      val offsetsOption =
        if (a.x >= b.x && a.y >= b.y)
          Some(((1, 1), (-1, -1)))
        else if (a.x <= b.x && a.y <= b.y) {
          Some(((-1, -1), (1, 1)))
        } else if (a.x >= b.x && a.y <= b.y)
          Some(((1, -1), (-1, 1)))
        else if (a.x <= b.x && a.y >= b.y)
          Some(((-1, 1), (1, -1)))
        else
          None

      val result: Set[Position] = offsetsOption
        .map(
          derivationFunction(a, b, colSize, rowSize, xDistance, yDistance, _)
        )
        .getOrElse(Set.empty)

      println(s"Anti nodes: $result")
      result
    }
  }

  def derivePart1AntiNodePositions(
      a: Position,
      b: Position,
      colSize: Int,
      rowSize: Int,
      xDistance: Int,
      yDistance: Int,
      offsets: ((Int, Int), (Int, Int))
  ): Set[Position] = {
    val antiNodes = Set(
      Position(
        a.x + xDistance * offsets._1._1,
        a.y + yDistance * offsets._1._2
      ),
      Position(b.x + xDistance * offsets._2._1, b.y + yDistance * offsets._2._2)
    )

    val result = antiNodes.filter(node =>
      node.x >= 0 && node.y >= 0 && node.x < colSize && node.y < rowSize
    )
    result
  }

  def derivePart2AntiNodePositions(
      a: Position,
      b: Position,
      colSize: Int,
      rowSize: Int,
      xDistance: Int,
      yDistance: Int,
      offsets: ((Int, Int), (Int, Int))
  ): Set[Position] =
    derivePart2AntiNodePositionsRec(
      a,
      b,
      colSize,
      rowSize,
      xDistance,
      yDistance,
      offsets,
      Set(a, b)
    )

  @tailrec
  private def derivePart2AntiNodePositionsRec(
      a: Position,
      b: Position,
      colSize: Int,
      rowSize: Int,
      xDistance: Int,
      yDistance: Int,
      offsets: ((Int, Int), (Int, Int)),
      accumulatedAntiNodes: Set[Position]
  ): Set[Position] = {

    val antiNodes = List(
      Position(
        a.x + xDistance * offsets._1._1,
        a.y + yDistance * offsets._1._2
      ),
      Position(b.x + xDistance * offsets._2._1, b.y + yDistance * offsets._2._2)
    )

    val validResults = antiNodes
      .zip(List(a, b))
      .map(pair =>
        if (
          pair._1.x >= 0 && pair._1.y >= 0 && pair._1.x < colSize && pair._1.y < rowSize
        ) pair._1
        else pair._2
      )

    if (validResults == List(a, b)) {
      accumulatedAntiNodes
    } else {
      val newAccumulatedAntiNodes = accumulatedAntiNodes ++ validResults
      derivePart2AntiNodePositionsRec(
        validResults.head,
        validResults.last,
        colSize,
        rowSize,
        xDistance,
        yDistance,
        offsets,
        newAccumulatedAntiNodes
      )
    }
  }
}

@main def main(): Unit = {

  val filename = "aoc2024-day8-input.txt"
  //  val filename = "test.txt"
  val antennaMap = loadData(filename)
  println(antennaMap)

  // Part 1
  val part1AntiNodes = antennaMap.antennaPairs
    .flatMap(
      _._2.flatMap(
        calculateAntiNodes(
          _,
          antennaMap.colSize,
          antennaMap.rowSize,
          derivePart1AntiNodePositions
        )
      )
    )
    .toSet
  println(s"Anti nodes size: ${part1AntiNodes.size}")

  // Part 2
  val part2AntiNodes = antennaMap.antennaPairs
    .flatMap(
      _._2.flatMap(
        calculateAntiNodes(
          _,
          antennaMap.colSize,
          antennaMap.rowSize,
          derivePart2AntiNodePositions
        )
      )
    )
    .toSet
  println(s"Anti nodes size: ${part2AntiNodes.size}")

}
