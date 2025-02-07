package aoc
package aoc2024.day21

import aoc2024.day21.Keypad2.{computeCost, computeCostForNumericSequence}
import common.Position
import common.Utils.loadData
import scala.collection.mutable.Map

object Keypad2 {

  val moveCosts = collection.mutable.Map
    .empty[(Position, Position, Int), Long]

  // Level 0 (pressed by human) cost
  private val costsByLevel: Map[(Char, Char, Int), Long] = Map.empty

  private val shortestSequences = Map(
    ('A', 'A') -> Seq("A"),
    ('A', '^') -> Seq("<A"),
    ('A', '<') -> Seq("v<<A"),
    ('A', 'v') -> Seq("v<A", "<vA"),
    ('A', '>') -> Seq("vA"),
    ('^', 'A') -> Seq(">A"),
    ('^', '^') -> Seq("A"),
    ('^', '<') -> Seq("v<A"),
    ('^', '>') -> Seq(">vA", "v>A"),
    ('<', 'A') -> Seq(">>^A"),
    ('<', '^') -> Seq(">^A"),
    ('<', '<') -> Seq("A"),
    ('<', 'v') -> Seq(">A"),
    ('v', 'A') -> Seq(">^A", "^>A"),
    ('v', '<') -> Seq("<A"),
    ('v', 'v') -> Seq("A"),
    ('v', '>') -> Seq(">A"),
    ('>', 'A') -> Seq("^A"),
    ('>', '^') -> Seq("^<A", "<^A"),
    ('>', 'v') -> Seq("<A"),
    ('>', '>') -> Seq("A")
  )

  private val numericKeypad: Map[Char, Position] = Map(
    '7' -> Position(0, 0),
    '8' -> Position(1, 0),
    '9' -> Position(2, 0),
    '4' -> Position(0, 1),
    '5' -> Position(1, 1),
    '6' -> Position(2, 1),
    '1' -> Position(0, 2),
    '2' -> Position(1, 2),
    '3' -> Position(2, 2),
    '0' -> Position(1, 3),
    'A' -> Position(2, 3)
  )

  // Compute costs from the lowest level upwards. Lowest level is based on shortestSequences
  def computeCost(maxLevel: Int): Unit = {
    shortestSequences.foreach((k, v) =>
      costsByLevel((k._1, k._2, 0)) = v.map(_.length).min
    )

    (0 until maxLevel).foreach { level =>
      shortestSequences.foreach((k, v) => {
        val minCost = v
          .map(keySeq => {
            ("A" + keySeq)
              .sliding(2)
              .map(s => {
                costsByLevel((s(0), s(1), level))
              })
              .sum
          })
          .min
        costsByLevel((k._1, k._2, level + 1)) = minCost
      })
    }

  }

  def computeCostForNumericSequence(keySequence: String, level: Int): Long = {
    val directionalSequences = ("A" + keySequence)
      .sliding(2)
      .map(s =>
        generateNumericKeypadSequences(numericKeypad(s(0)), numericKeypad(s(1)))
      )

    // Derive all possible path combinations.
    // Needed because some (from, to) pairs have multiple shortest paths of same length.
    val combinations: Seq[String] = directionalSequences.foldLeft(Seq("A")) {
      (acc, seq) =>
        acc.flatMap(prefix => seq.map(s => s"$prefix$s"))
    }

    scribe.debug(s"$combinations")

    combinations
      .map(sequence =>
        sequence
          .sliding(2)
          .map(pair => costsByLevel((pair(0), pair(1), level)))
          .sum
      ).min
  }

  private def generateNumericKeypadSequences(
      fromPosition: Position,
      toPosition: Position
  ): Seq[String] = {

    val deltaX = toPosition.x - fromPosition.x
    val deltaY = toPosition.y - fromPosition.y

    val horizontalDirection = if (deltaX > 0) ">" else "<"
    val verticalDirection = if (deltaY > 0) "v" else "^"
    val horizontalMovements = horizontalDirection * deltaX.abs
    val verticalMovements = verticalDirection * deltaY.abs

    val moveInstructions =
      if (fromPosition.x == 0 && toPosition.y == 3)
        Seq(horizontalMovements + verticalMovements)
      else if (fromPosition.y == 3 && toPosition.x == 0)
        Seq(verticalMovements + horizontalMovements)
      else if (horizontalMovements.isEmpty)
        Seq(verticalMovements)
      else if (verticalMovements.isEmpty)
        Seq(horizontalMovements)
      else
        Seq(
          horizontalMovements + verticalMovements,
          verticalMovements + horizontalMovements
        )

    moveInstructions.map(_ + 'A')
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
  val filename = "aoc2024-day21-input.txt"
  val instructions = loadData(filename)(_.toSeq)
  val numbers = instructions.map(_.dropRight(1)).map(Integer.parseInt)
  scribe.debug(numbers.toString)

  computeCost(25)

  var costs = instructions.map(instruction =>
    computeCostForNumericSequence(instruction, 1)
  )
  scribe.debug(s"$costs")

  var pairs = costs.zip(numbers)
  scribe.debug(pairs.toString)

  var complexity = pairs.map(_ * _).sum
  scribe.info(s"Part 1 Complexity is $complexity")

  costs = instructions.map(instruction =>
    computeCostForNumericSequence(instruction, 24)
  )
  scribe.debug(s"$costs")

  pairs = costs.zip(numbers)
  scribe.debug(pairs.toString)

  complexity = pairs.map(_ * _).sum
  scribe.info(s"Part 2 Complexity is $complexity")
}
