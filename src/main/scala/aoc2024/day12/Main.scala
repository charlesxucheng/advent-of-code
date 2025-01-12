package aoc
package aoc2024.day12

import common.Utils.loadData
import common.{Position, TwoDMap}
import scala.annotation.tailrec

object PlantRegion {
  def findAllPlantRegions(map: TwoDMap[Char]): Set[PlantRegion] =
    findPlantRegionsRec(map, map.allPositions, Set.empty)

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

  @tailrec
  private def findPlantRegionsRec(
      map: TwoDMap[Char],
      positions: Seq[Position],
      accumulator: Set[PlantRegion]
  ): Set[PlantRegion] = {
    positions match {
      case Seq() => accumulator
      case head +: tail =>
        println(
          s"Finding region for ${head.displayAsArrayElement} (${map.get(head)})"
        )
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
  def perimeter: Int = area * 4 - allStrictNeighbors.size

  def area: Int = positions.size

  private def allStrictNeighbors: Seq[Position] =
    positions.toSeq.flatMap(strictNeighbors)

  private def strictNeighbors(position: Position): Set[Position] = {
    require(positions.contains(position))
    position.cardinalPositions.intersect(positions)
  }

  def sides: Int =
    if (area == 1) 4
    else
      inflate.allRelaxedNeighbors
        .map(_.size match {
          case 3 | 4 | 7 => true
          case _         => false
        })
        .count(_ == true)

  private def allRelaxedNeighbors: Seq[Set[Position]] =
    positions.toSeq.map(p => relaxedNeighbors(p))

  private def relaxedNeighbors(position: Position): Set[Position] = {
    require(positions.contains(position))
    position.neighboringPositions.intersect(positions)
  }

  private def inflate: PlantRegion =
    PlantRegion(
      positions.flatMap(p =>
        Set(
          Position(p.x * 2, p.y * 2),
          Position(p.x * 2 + 1, p.y * 2),
          Position(p.x * 2, p.y * 2 + 1),
          Position(p.x * 2 + 1, p.y * 2 + 1)
        )
      ),
      map
    )

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
      val numberOfSides = region.sides
      println(
        s"Region: ${map.get(region.positions.head)} - Are: $area, Perimeter: $perimeter, # Sides: $numberOfSides, Cost1: ${area * perimeter}, Cost2: ${area * numberOfSides}"
      )
      (area * perimeter, area * numberOfSides)
    }
  }

  println(s"Part 1 Total Cost: ${costs.map(_._1).sum}")
  println(s"Part 2 Total Cost: ${costs.map(_._2).sum}")
}
