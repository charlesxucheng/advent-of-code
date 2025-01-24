package aoc
package aoc2024.day18

import common.Utils.loadData

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Debug))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day18-input.txt"
  val input = loadData(filename)(RAMMap.parseInput)

  val map = RAMMap(71, 71, input.take(1024))
//  val map = RAMMap(7, 7, input.take(12))

  val result = RAMMap.shortestPath(map)

  scribe.info(s"The shortest path is $result")

}
