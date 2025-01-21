package aoc
package aoc2024.day17

import scala.annotation.tailrec

case class Operand(value: Int) {
  require(value >= 0 && value <= 7)
}

object Computer {

  def parseInput(input: Iterator[String]): Option[Computer] = {
    input.toSeq.filter(_ != "") match
      case Seq(
            s"Register A: ${aValue}",
            s"Register B: ${bValue}",
            s"Register C: ${cValue}",
            s"Program: ${instructions}"
          ) =>
        Some(
          Computer(
            registerA = aValue.toLong,
            registerB = bValue.toLong,
            registerC = cValue.toLong,
            instructions = instructions.split(",").map(_.toInt).toSeq,
            instructionPointer = 0
          )
        )
      case _ => None
  }

  def findNumberForPart2(): Seq[Long] = {
    val program = Seq(2, 4, 1, 2, 7, 5, 4, 5, 1, 3, 5, 5, 0, 3, 3, 0).reverse

    def outputMatchesProgramCode(registerA: Long, expectedOutput: Int): Boolean = {
      var registerB = (registerA % 8) ^ 0b010
      val registerC = registerA >> registerB
      registerB = registerB ^ registerC ^ 0b011
      registerB % 8 == expectedOutput
    }

    @tailrec
    def findBitsRec(step: Int, accumulatedInput: Seq[Long]): Seq[Long] = {
      if (accumulatedInput.isEmpty)
        scribe.warn(s"At step $step, no suitable solutions are found")
      if (step == 16) accumulatedInput
      else {
        val a = for {
          bits <- 0 to 7
          accumulated <- accumulatedInput
          appended = appendBits(accumulated, bits)
          if outputMatchesProgramCode(appended, program(step))
        } yield appended

        findBitsRec(step + 1, a)
      }
    }

    findBitsRec(0, Seq(0L))
  }

  private def appendBits(input: Long, bits: Int): Long = (input << 3) | bits
}

case class Computer(
    registerA: Long,
    registerB: Long,
    registerC: Long,
    instructions: Seq[Int],
    instructionPointer: Int
) {

  private val opcodes: Map[Int, Operand => Computer] = Map(
    0 -> adv,
    1 -> bxl,
    2 -> bst,
    3 -> jnz,
    4 -> bxc,
    5 -> out,
    6 -> bdv,
    7 -> cdv
  )

  def runUntilEnd(): Unit = {
    var currentComputer = this
    while (!currentComputer.completed) {
      currentComputer = currentComputer.runStep()
    }
  }

  private def completed: Boolean = instructionPointer >= instructions.size

  private def runStep(): Computer =
    val instruction = instructions(instructionPointer)
    val operand = instructions(instructionPointer + 1)
    opcodes
      .get(instruction)
      .map(_ apply Operand(operand))
      .getOrElse(throw new IllegalArgumentException("Invalid input format."))

  private def adv(operand: Operand): Computer =
    this.copy(registerA = rightShift(operand)).nextInstruction()

  private def bxl(operand: Operand): Computer =
    this.copy(registerB = registerB ^ operand.value).nextInstruction()

  private def bst(operand: Operand): Computer =
    this.copy(registerB = combo(operand) % 8).nextInstruction()

  private def jnz(operand: Operand): Computer =
    if (registerA == 0) this.nextInstruction()
    else this.copy(instructionPointer = operand.value)

  private def bxc(operand: Operand): Computer =
    this.copy(registerB = registerB ^ registerC).nextInstruction()

  private def out(operand: Operand): Computer = {
    val output = combo(operand) % 8
    print(s"$output,")
    this.nextInstruction()
  }

  private def bdv(operand: Operand): Computer =
    this.copy(registerB = rightShift(operand)).nextInstruction()

  private def cdv(operand: Operand): Computer =
    this.copy(registerC = rightShift(operand)).nextInstruction()

  private def nextInstruction(): Computer =
    this.copy(instructionPointer = instructionPointer + 2)

  private def rightShift(operand: Operand): Long =
    registerA >> combo(operand)

  private def combo(operand: Operand): Long = operand match {
    case Operand(0 | 1 | 2 | 3) => operand.value
    case Operand(4)             => registerA
    case Operand(5)             => registerB
    case Operand(6)             => registerC
    case _ =>
      throw UnsupportedOperationException(
        "Combo Operand value 7 is reserved and should not appear in valid programs."
      )
  }

}
