package aoc
package aoc2024.day17

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
            registerA = aValue.toInt,
            registerB = bValue.toInt,
            registerC = cValue.toInt,
            instructions = instructions.split(",").map(_.toInt).toSeq,
            instructionPointer = 0
          )
        )
      case _ => None

  }
}

case class Computer(
    registerA: Int,
    registerB: Int,
    registerC: Int,
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
    if (registerA == 0) this
    else this.copy(instructionPointer = operand.value)

  private def bxc(operand: Operand): Computer =
    this.copy(registerB = registerB ^ registerC).nextInstruction()

  private def nextInstruction(): Computer =
    this.copy(instructionPointer = instructionPointer + 2)

  private def out(operand: Operand): Computer = {
    val output = combo(operand) % 8
    print(s"$output,")
    this.nextInstruction()
  }

  private def combo(operand: Operand): Int = operand match {
    case Operand(0 | 1 | 2 | 3) => operand.value
    case Operand(4)             => registerA
    case Operand(5)             => registerB
    case Operand(6)             => registerC
    case _ =>
      throw UnsupportedOperationException(
        "Combo Operand value 7 is reserved and should not appear in valid programs."
      )
  }

  private def bdv(operand: Operand): Computer =
    this.copy(registerB = rightShift(operand)).nextInstruction()

  private def cdv(operand: Operand): Computer =
    this.copy(registerC = rightShift(operand)).nextInstruction()

  private def rightShift(operand: Operand): Int =
    registerA >> combo(operand)

}
