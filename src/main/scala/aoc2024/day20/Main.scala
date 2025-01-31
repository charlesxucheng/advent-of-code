package aoc
package aoc2024.day20

import aoc2024.day20.Race.*
import common.Utils.loadData
import common.{Position, TwoDMap}
import scala.annotation.tailrec

object Race {

  def createMap(map: TwoDMap[Char]): RaceMap = {
    val startPosition = map.findFirstPosition('S').get
    val endPosition = map.findFirstPosition('E').get
    val walls = map.findAll('#').toSeq
    RaceMap(
      map.getColRange.size,
      map.getRowRange.size,
      startPosition,
      endPosition,
      walls
    )
  }

  extension (p: Position) {
    def minDistance(other: Position): Int = {
      val (dx, dy) = p - other
      dx.abs + dy.abs
    }
  }
}

case class CheatImpact(from: Position, to: Position, saving: Int)

case class RaceMap(
    colSize: Int,
    rowSize: Int,
    startPosition: Position,
    endPosition: Position,
    walls: Seq[Position]
) {

  def contains(position: Position): Boolean =
    position.x >= 0 && position.x < colSize && position.y >= 0 && position.y < rowSize

  @tailrec
  final def findPath(
      currentPosition: Position,
      visited: IndexedSeq[Position]
  ): IndexedSeq[Position] =
    if (currentPosition == endPosition)
      visited :+ currentPosition
    else {
      val nextPositions = currentPosition.cardinalPositions.filterNot(p =>
        walls.contains(p) || visited.contains(p)
      )
      assert(nextPositions.size == 1)
      findPath(nextPositions.head, visited :+ currentPosition)
    }

  def findCheats(cheatDuration: Int, savingsThreshold: Int): Long = {

    @tailrec
    def getAllPositionsAfterCheat(
        positions: Set[Position],
        cheatDuration: Int,
        accumulated: Set[Position]
    ): Set[Position] =
      if (cheatDuration == 0) accumulated
      else
        getAllPositionsAfterCheat(
          positions.flatMap(_.cardinalPositions.filter(p => contains(p))),
          cheatDuration - 1,
          accumulated ++ positions.flatMap(
            _.cardinalPositions.filter(p => contains(p))
          )
        )

    def assessCheatImpact(
        position: Position,
        index: Int,
        indexedPath: Map[Position, Int]
    ): Set[CheatImpact] =
      getAllPositionsAfterCheat(Set(position), cheatDuration, Set.empty)
        .filter(p => indexedPath.exists(p == _._1))
        .flatMap(p => {
          val index1 = indexedPath(p)
          val index2 = indexedPath(position)
          val saving = (index1 - index2) - p.minDistance(position)
          if (saving > 0) Some(CheatImpact(position, p, saving)) else None
        })

    val path = findPath(startPosition, IndexedSeq.empty)
    val indexedPath = path.zipWithIndex.toMap
    val cheatImpacts = indexedPath.flatMap((position, index) => {
      scribe.info(s"Processing Position $position at index $index")
      assessCheatImpact(position, index, indexedPath)
    })

    val savings =
      cheatImpacts.filter(ci => ci.saving >= savingsThreshold).map(_.saving)
    scribe.debug(
      s"Impacts: ${savings.groupBy(identity).map { case (k, v) => k -> v.size }}"
    )
    savings.size
  }

}

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Info))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day20-input.txt"
  val map = loadData(filename)(TwoDMap.parseInput(identity))
  map.display()

  val raceMap = Race.createMap(map)
  val path = raceMap.findPath(raceMap.startPosition, IndexedSeq.empty)

  scribe.debug(path.toString())

  val part1SavingThreshold = 100
  val part2SavingThreshold = 100

  val part1Count = raceMap.findCheats(2, part1SavingThreshold)
  scribe.info(
    s"Part 1 - Number of cheats that would save at least $part2SavingThreshold picoseconds: $part1Count"
  )
  val part2Count = raceMap.findCheats(20, part2SavingThreshold)
  scribe.info(
    s"Part 2 Number of cheats that would save at least $part2SavingThreshold picoseconds: $part2Count"
  )

}
