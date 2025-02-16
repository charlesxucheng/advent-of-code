package aoc
package aoc2024.day24

import aoc2024.day24.Gate.simulateGates
import common.Utils.loadData
import scala.collection.mutable
import scala.collection.mutable.{Map, Queue}

type Wire = String
type Operation = (Boolean, Boolean) => Boolean

case class Gate(
    input1: Wire,
    input2: Wire,
    output: Wire,
    operation: Operation
)

object Gate {
  private val AND: Operation = (a, b) => a && b
  private val OR: Operation = (a, b) => a || b
  private val XOR: Operation = (a, b) => a != b

  def parseInput(input: Iterator[String]): (Map[Wire, Boolean], Queue[Gate]) = {
    val initialValues = Map.empty[Wire, Boolean]

    input
      .takeWhile(_.contains(":"))
      .map { line =>
        val parts = line.split(":")
        parts(0).trim -> (parts(1).trim.toInt == 1)
      }
      .foreach { case (wire, value) => initialValues.put(wire, value) }

    val gates =
      input.dropWhile(_.contains(":")).filter(_.nonEmpty).map { line =>
        val parts = line.split("->")
        val gateParts = parts(0).trim.split(" ")
        val input1 = gateParts(0)
        val operation = gateParts(1) match {
          case "AND" => AND
          case "OR"  => OR
          case "XOR" => XOR
        }
        val input2 = gateParts(2)
        val output = parts(1).trim
        Gate(input1, input2, output, operation)
      }

    (initialValues, Queue(gates.toSeq*))
  }

  def simulateGates(
      gates: Queue[Gate],
      initialValues: Map[Wire, Boolean]
  ): Map[Wire, Boolean] = {
    val wireValues = initialValues

    while (gates.nonEmpty) {
      val gate = gates.dequeue()
      if (
        wireValues
          .contains(gate.input1) && wireValues.contains(gate.input2)
      ) {
        val input1 = wireValues(gate.input1)
        val input2 = wireValues(gate.input2)
        val output = gate.operation(input1, input2)
        wireValues(gate.output) = output
      } else gates.enqueue(gate)
    }
    wireValues
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
  val filename = "aoc2024-day24-input.txt"
  val input = loadData(filename)(Gate.parseInput)
  scribe.debug(s"${input._1}")
  scribe.debug(s"${input._2}")

  val wireValues = simulateGates(input._2, input._1)
  scribe.debug(s"${wireValues.filter(_._1.startsWith("z"))}")

  val result = wireValues
    .filter(_._1.startsWith("z"))
    .toSeq
    .sortBy(_._1)
    .reverse
    .map { case (wire, value) => if (value) "1" else "0" }
    .mkString("")

  scribe.info(s"Part 1 result: ${BigInt(result, 2)}")

}
