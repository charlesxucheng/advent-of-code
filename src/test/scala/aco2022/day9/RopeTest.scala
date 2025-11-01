package aoc
package aco2022.day9

import aco2022.day9.RopeBridge.Rope
import common.MovementDirection.*
import common.Position

import aoc2022.UnitSpec
import org.scalatest.matchers.should.Matchers.*

class RopeTest extends UnitSpec {
  "A Rope" when {
    "created" should {
      "have head and tail not more than 1 position apart vertically and horizontally" in {
        val validRopes = Table(
          ("Head x", "Head y", "Tail x", "Tail y"),
          (0, 0, 0, 0),
          (-1, -1, -1, -1),
          (2, 2, 2, 2),
          (1, 1, 1, 2),
          (1, 1, 1, 0),
          (2, 5, 3, 5),
          (2, 5, 1, 5),
          (2, 5, 3, 6),
          (2, 5, 3, 4),
          (2, 5, 1, 4),
          (2, 5, 1, 6),
          (-5, -5, -4, -4)
        )

        val invalidRopes = Table(
          ("Head x", "Head y", "Tail x", "Tail y"),
          (0, 0, 2, 2),
          (0, 0, 1, 3),
          (5, 4, 9, 8),
          (-1, -2, 5, 6)
        )

        forAll(validRopes) { (hx, hy, tx, ty) =>
          val rope = Rope(Position(hx, hy), Position(tx, ty))
          rope.head.x shouldBe hx
          rope.head.y shouldBe hy
          rope.tail.x shouldBe tx
          rope.tail.y shouldBe ty
        }

        forAll(invalidRopes) { (hx, hy, tx, ty) =>
          intercept[IllegalArgumentException] {
            Rope(Position(hx, hy), Position(tx, ty))
          }
        }
      }
    }

    "move its head by one step while the tail is overlapping with the head" should {
      "move the head but not the tail" in {
        val rope = Rope(Position(0, 0), Position(0, 0))
        rope.move(Up) shouldBe Rope(Position(0, 1), Position(0, 0))
        rope.move(Down) shouldBe Rope(Position(0, -1), Position(0, 0))
        rope.move(Right) shouldBe Rope(Position(1, 0), Position(0, 0))
        rope.move(Left) shouldBe Rope(Position(-1, 0), Position(0, 0))
      }
    }

    "move its head by one step away from the tail while the tail is next to the head vertically" should {
      "move the head and the tail by one step each in the same direction" in {
        val testData = Table(
          (
            "Head x",
            "Head y",
            "Tail x",
            "Tail y",
            "Movement",
            "Expected head x",
            "Expected head y",
            "Expected tail x",
            "Expected tail y"
          ),
          (1, 1, 2, 1, Left, 0, 1, 1, 1),
          (3, 2, 2, 2, Right, 4, 2, 3, 2)
        )

        forAll(testData) { (hx, hy, tx, ty, m, ex, ey, etx, ety) =>
          val rope = Rope(Position(hx, hy), Position(tx, ty))
          rope.move(m) shouldBe Rope(Position(ex, ey), Position(etx, ety))
        }
      }
    }

    "move its head by one step away from the tail while the tail is next to the head horizontally" should {
      "move the head and the tail by one step each in the same direction" in {
        val testData = Table(
          (
            "Head x",
            "Head y",
            "Tail x",
            "Tail y",
            "Movement",
            "Expected head x",
            "Expected head y",
            "Expected tail x",
            "Expected tail y"
          ),
          (1, 1, 1, 2, Down, 1, 0, 1, 1),
          (2, 3, 2, 2, Up, 2, 4, 2, 3)
        )

        forAll(testData) { (hx, hy, tx, ty, m, ex, ey, etx, ety) =>
          val rope = Rope(Position(hx, hy), Position(tx, ty))
          rope.move(m) shouldBe Rope(Position(ex, ey), Position(etx, ety))
        }
      }
    }

    "move its head to the tail" should {
      "not cause the tail to move" in {
        val testData = Table(
          (
            "Head x",
            "Head y",
            "Tail x",
            "Tail y",
            "Movement",
            "Expected head x",
            "Expected head y"
          ),
          (1, 1, 1, 2, Up, 1, 2),
          (1, 1, 0, 1, Left, 0, 1),
          (1, 1, 2, 1, Right, 2, 1),
          (1, 1, 1, 0, Down, 1, 0)
        )

        forAll(testData) { (hx, hy, tx, ty, m, ex, ey) =>
          val rope = Rope(Position(hx, hy), Position(tx, ty))
          rope.move(m) shouldBe Rope(Position(ex, ey), Position(tx, ty))
        }
      }
    }

    "move its head away from the tail while the two are diagonally positioned" should {
      "move the tail to the original position of the head" in {
        val testData = Table(
          (
            "Head x",
            "Head y",
            "Tail x",
            "Tail y",
            "Movement",
            "Expected head x",
            "Expected head y"
          ),
          (1, 1, 2, 2, Left, 0, 1),
          (1, 1, 2, 2, Down, 1, 0)
        )

        forAll(testData) { (hx, hy, tx, ty, m, ex, ey) =>
          val rope = Rope(Position(hx, hy), Position(tx, ty))
          rope.move(m) shouldBe Rope(Position(ex, ey), Position(hx, hy))
        }
      }
    }

    "move its head 'around' the tail so that the two are still next to each other (including diagonally)" should {
      "not cause the tail to move" in {
        val testData = Table(
          (
            "Head x",
            "Head y",
            "Tail x",
            "Tail y",
            "Movement",
            "Expected head x",
            "Expected head y"
          ),
          (1, 1, 2, 2, Up, 1, 2),
          (1, 1, 2, 2, Right, 2, 1),
          (2, 2, 1, 1, Down, 2, 1),
          (2, 2, 1, 1, Left, 1, 2),
          (1, 2, 2, 1, Right, 2, 2),
          (1, 2, 2, 1, Down, 1, 1),
          (2, 1, 1, 2, Left, 1, 1),
          (2, 1, 1, 2, Up, 2, 2),
          (1, 1, 2, 1, Up, 1, 2),
          (1, 1, 2, 1, Down, 1, 0),
          (2, 1, 1, 1, Up, 2, 2),
          (2, 1, 1, 1, Down, 2, 0)
        )

        forAll(testData) { (hx, hy, tx, ty, m, ex, ey) =>
          val rope = Rope(Position(hx, hy), Position(tx, ty))
          rope.move(m) shouldBe Rope(Position(ex, ey), Position(tx, ty))
        }
      }
    }
  }

}
