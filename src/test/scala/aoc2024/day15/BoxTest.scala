package aoc
package aoc2024.day15

import aoc2022.UnitSpec
import aoc2024.day15.MoveInstruction.*
import common.Position
import org.scalatest.matchers.should.Matchers.*


class BoxTest extends UnitSpec {
  "A Box's Reference Positions" should {
    "be the far side of the MoveInstruction it received" in {
      val verticalBox = DoubleCellBox(Position(3, 8), Position(3, 9))
      verticalBox.getTargetPositions(Up) shouldBe Set(Position(3,7))
      verticalBox.getTargetPositions(Down) shouldBe Set(Position(3,10))
      verticalBox.getTargetPositions(Left) shouldBe Set(Position(2,8), Position(2,9))
      verticalBox.getTargetPositions(Right) shouldBe Set(Position(4,8), Position(4,9))

      val horizontalBox = DoubleCellBox(Position(3, 8), Position(4, 8))
      horizontalBox.getTargetPositions(Up) shouldBe Set(Position(3,7), Position(4,7))
      horizontalBox.getTargetPositions(Down) shouldBe Set(Position(3,9), Position(4,9))
      horizontalBox.getTargetPositions(Left) shouldBe Set(Position(2,8))
      horizontalBox.getTargetPositions(Right) shouldBe Set(Position(5,8))
    }
  }

}
