package aoc
package aoc2024.day12

import common.Utils.loadData
import common.{Position, TwoDMap}
import scala.annotation.tailrec

object PlantRegion {
  @tailrec
  private def fillRegion(
      positions: Set[Position],
      map: TwoDMap[Char],
      accumulator: Set[Position]
  ): Set[Position] = {
    require(
      positions.forall(map.contains) && positions
        .map(p => map.get(p))
        .size == 1
    )

    val neighborsWithSameValue = positions.flatMap(position =>
      position.cardinalPositions.filter(p =>
        !accumulator.contains(p) && map.contains(p) && map.get(
          positions.head
        ) == map.get(p)
      )
    )

    if (neighborsWithSameValue.isEmpty)
      accumulator
    else
      fillRegion(
        neighborsWithSameValue,
        map,
        accumulator ++ neighborsWithSameValue
      )
  }

  def findAllPlantRegions(map: TwoDMap[Char]): Set[PlantRegion] =
    findPlantRegionsRec(map, map.allPositions, Set.empty)

  @tailrec
  private def findPlantRegionsRec(
      map: TwoDMap[Char],
      positions: Seq[Position],
      accumulator: Set[PlantRegion]
  ): Set[PlantRegion] = {
    positions match {
      case Seq() => accumulator
      case head +: tail =>
        println(s"Finding region for $head (${map.get(head)})")
        if (accumulator.exists(_.positions.contains(head)))
          findPlantRegionsRec(map, tail, accumulator)
        else {
          val newRegion = PlantRegion.fillRegion(Set(head), map, Set(head))
          findPlantRegionsRec(
            map,
            tail,
            accumulator + PlantRegion(newRegion, map)
          )
        }
    }
  }

}

case class PlantRegion(positions: Set[Position], map: TwoDMap[Char]) {
  def area: Int = positions.size
  def perimeter: Int = area * 4 - allNeighbors.size

  private def neighbors(position: Position): Set[Position] = {
    require(positions.contains(position))
    position.cardinalPositions.intersect(positions)
  }

  private def allNeighbors: Seq[Position] = positions.toSeq.flatMap(neighbors)

}

@main def main(): Unit = {

  val filename = "aoc2024-day12-input.txt"
//  val filename = "test.txt"

  val map = loadData(filename)(TwoDMap.parseInput(identity[Char]))
  map.display()

  val plantRegions = PlantRegion.findAllPlantRegions(map)
  val costs = plantRegions.toSeq.map { region =>
    {
      val area = region.area
      val perimeter = region.perimeter
      println(
        s"Region: ${map.get(region.positions.head)} - Are: $area, Perimeter: $perimeter, Product: ${area * perimeter}"
      )
      area * perimeter
    }
  }

  println(s"Part 1 Total Cost: ${costs.sum}")
}
