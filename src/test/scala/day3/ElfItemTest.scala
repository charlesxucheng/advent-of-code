package aoc2022
package day3

import org.scalatest.matchers.should.Matchers._

import ElfItem.*

class ElfItemTest extends UnitSpec {
  "An ElfItem" should {
    "can be constructed from a string" in {
      val testData = Seq('a', 'z', 'A', 'Z', 'f', 'H')
      testData.foreach { t =>
        ElfItem(t) shouldBe an [ElfItem]
        ElfItem(t).toChar shouldBe t
      }

    }
    "cannot be constructed from an invalid Char" in {
      a [IllegalArgumentException] should be thrownBy ElfItem('%')
    }
  }
}
