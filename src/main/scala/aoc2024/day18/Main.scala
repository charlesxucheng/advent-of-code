package aoc
package aoc2024.day18

import common.Utils.loadData

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Info))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day18-input.txt"
  val input = loadData(filename)(RAMMap.parseInput)
  //  val map = RAMMap(7, 7, input.take(12))
  val map = RAMMap(71, 71, input.take(1024))

  val result = RAMMap.shortestPath(map)

  scribe.info(s"The shortest path is $result")

  // Part 2
  val mapWithAllCorruptions = RAMMap(71, 71, input)
  val blockingNumber =
    RAMMap.allPathBlockedAt(mapWithAllCorruptions, 1024, input.size - 1)

  scribe.info(
    s"The number of corruptions to block all paths is ${input(blockingNumber)}"
  )
}
