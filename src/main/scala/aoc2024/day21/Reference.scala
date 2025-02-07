package aoc
package aoc2024.day21

import aoc2024.day21.Keypad.bestMoveSequenceCost
import common.Utils.loadData

// Solution based on the sample solution.

@main
def oldMain(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Debug))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day21-input.txt"
  val instructions = loadData(filename)(_.toSeq)
  val numbers = instructions.map(_.dropRight(1)).map(Integer.parseInt)
  scribe.debug(numbers.toString)

  val costs =
    instructions
      .map(instruction => bestMoveSequenceCost(instruction, 0, 25))

  val pairs = costs.zip(numbers)
  scribe.debug(pairs.toString)

  val complexity = pairs.map(_ * _).sum
  scribe.info(s"Part 1 Complexity is $complexity")
}
