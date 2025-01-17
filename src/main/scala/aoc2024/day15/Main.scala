package aoc
package aoc2024.day15

import aoc2024.day15.WarehouseMap.*
import common.TwoDMap
import common.Utils.loadData

@main def main(): Unit = {

  val mapFilename = "aoc2024-day15-input1.txt"
//  val mapFilename = "test.txt"
  val instructionsFilename = "aoc2024-day15-input2.txt"
//  val instructionsFilename = "test2.txt"

  val map = loadData(mapFilename)(TwoDMap.parseInput(identity))
  map.display()
  val warehouseMap = WarehouseMap(map)
  scribe.info(
    s"Robot position: ${warehouseMap.robotPosition.displayAsArrayElement}"
  )

  val instructions = loadData(instructionsFilename)(MoveInstruction.parseInput)
  scribe.info(instructions.toString())

  val resultMap = instructions.foldLeft(warehouseMap)((map, instruction) => {
    map.moveRobot(instruction)
  })

  scribe.info(s"GPS Coordinate of all boxes = ${resultMap.gpsCoordinate}")

  // Part 2
  scribe.info("Part 2")
  val widenedMap = map.widen()
  widenedMap.display()

  val warehouseMap2 = WarehouseMap(widenedMap)
  scribe.info(
    s"Robot position: ${warehouseMap2.robotPosition.displayAsArrayElement}"
  )

  val resultMap2 = instructions.foldLeft(warehouseMap2)((map, instruction) => {
    map.moveRobot(instruction)
  })

  scribe.info(s"GPS Coordinate of all boxes = ${resultMap2.gpsCoordinate}")
}
