package aoc
package aoc2024.day24

import aoc2024.day24.Gate.simulateGates
import aoc2024.day24.OperationName.{AND, OR, XOR}
import common.Utils.loadData

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.collection.{immutable, mutable}

type Wire = String
type Operation = (Boolean, Boolean) => Boolean

enum OperationName {
  case AND, OR, XOR
}

case class GateInput(input1: Wire, input2: Wire) {
  override def equals(obj: Any): Boolean = obj match {
    case GateInput(a, b) =>
      (input1 == a && input2 == b) || (input1 == b && input2 == a)
    case _ => false
  }

  override def hashCode(): Int = input1.hashCode + input2.hashCode
}

case class Gate(
    input: GateInput,
    output: Wire,
    operation: OperationName
)

object Gate {

  private val operations: mutable.Map[OperationName, Operation] = mutable.Map(
    AND -> ((a, b) => a && b),
    OR -> ((a, b) => a || b),
    XOR -> ((a, b) => a != b)
  )

  def parseInput(
      input: Iterable[String]
  ): (mutable.Map[Wire, Boolean], mutable.Queue[Gate]) = {
    val initialValues = mutable.Map.empty[Wire, Boolean]

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
        val input2 = gateParts(2)
        val output = parts(1).trim
        Gate(input1, input2, output, OperationName.valueOf(gateParts(1)))
      }

    (initialValues, mutable.Queue(gates.toSeq*))
  }

  def apply(
      input1: Wire,
      input2: Wire,
      output: Wire,
      operation: OperationName
  ) =
    new Gate(GateInput(input1, input2), output, operation)

  def simulateGates(
      gates: mutable.Queue[Gate],
      initialValues: mutable.Map[Wire, Boolean]
  ): mutable.Map[Wire, Boolean] = {
    val wireValues = initialValues

    while (gates.nonEmpty) {
      val gate = gates.dequeue()
      if (
        wireValues
          .contains(gate.input.input1) && wireValues.contains(gate.input.input2)
      ) {
        val input1 = wireValues(gate.input.input1)
        val input2 = wireValues(gate.input.input2)
        val output = operations(gate.operation)(input1, input2)
        wireValues(gate.output) = output
      } else gates.enqueue(gate)
    }
    wireValues
  }

  def findInvalidGates(gates: List[Gate]): List[(Gate, String)] = {
    val gateMap = buildMap(gates)

    @tailrec
    def checkGates(
        bit: Int,
        carry: Wire,
        invalidGates: List[(Gate, String)]
    ): List[(Gate, String)] = {
      if (bit == 46) invalidGates
      else {
        val input1 = formString("x", bit)
        val input2 = formString("y", bit)

        val outWires = for {
          xor1 <- gateMap.get(GateInput(input1, input2), XOR)
          xor2 <- gateMap.get((GateInput(carry, xor1), XOR))
          and1 <- gateMap.get((GateInput(carry, xor1), AND))
          and2 <- gateMap.get((GateInput(input1, input2), AND))
          or1 <- gateMap.get((GateInput(and1, and2), OR))
        } yield {
          scribe.debug(s"Input = $input1 $input2, output=$xor2, Carry=$or1")
          (xor1, xor2, or1)
        }

        outWires match {
          case Some((xor1, xor2, or1)) =>
            if (bit < 45 && xor2 != formString("z", bit))
              checkGates(
                bit + 1,
                or1,
                (
                  Gate(input1, input2, xor1, XOR),
                  s"Output of bit $bit is wrong ($xor2)"
                ) :: invalidGates
              )
            else
              checkGates(bit + 1, or1, invalidGates)
          case None =>
            checkGates(
              bit + 1,
              "NA",
              (
                Gate(input1, input2, "NA", XOR),
                s"Cannot find operation for $input1, $input2"
              )
                :: invalidGates
            )
        }
      }
    }

    val carry1 = gateMap((GateInput("x00", "y00"), AND))
    checkGates(1, carry1, List.empty)
  }

  private def buildMap(
      gates: List[Gate]
  ): immutable.Map[(GateInput, OperationName), Wire] = {
    gates.map { gate =>
      ((gate.input, gate.operation), gate.output)
    }.toMap
  }

  private inline def formString(prefix: String, number: Int) =
    prefix + "%02d".format(number)

}

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Debug))
    .replace()

//  val filename = "test.txt"
//  val filename = "aoc2024-day24-corrected-input.txt"
  val filename = "aoc2024-day24-input.txt"
  val input = loadData(filename)(Gate.parseInput)
  scribe.debug(s"${input._1}")
  scribe.debug(s"${input._2}")

  val wireValues = simulateGates(input._2.clone(), input._1)
  scribe.debug(s"${wireValues.filter(_._1.startsWith("z"))}")

  val result = wireValues
    .filter(_._1.startsWith("z"))
    .toSeq
    .sortBy(_._1)
    .reverse
    .map { case (wire, value) => if (value) "1" else "0" }
    .mkString("")

  scribe.info(s"Part 1 result: ${BigInt(result, 2)}")

  val invalidGates = Gate.findInvalidGates(input._2.toList)

  invalidGates.foreach(ig => println(s"${ig._1}, ${ig._2}"))

  scribe.info(
    s"${List("qwf", "cnk", "vhm", "z14", "mps", "z27", "msq", "z39").sorted.mkString(",")}"
  )

}
