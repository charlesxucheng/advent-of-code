package aoc
package aco2022.day3

import aco2022.day3.ElfItem.*

import aoc2022.UnitSpec
import org.scalatest.matchers.should.Matchers.*

class ElfItemTest extends UnitSpec {
  "An ElfItem" should {
    "can be constructed from a valid Char" in {
      val testData = Seq('a', 'z', 'A', 'Z', 'f', 'H')
      testData.foreach { t =>
        ElfItem(t) shouldBe an [ElfItem]
        ElfItem(t).toChar shouldBe t
      }

    }
    "cannot be constructed from an invalid Char" in {
      val testData = Seq('&', ' ', '+', '\u3333')
      testData.foreach { a [IllegalArgumentException] should be thrownBy ElfItem(_) }
    }

    "have its priority value" in {
      val testData = Table(
        ("ElfItem", "Priority"),
        ('a', 1),
        ('z', 26),
        ('A', 27),
        ('Z', 52),
        ('g', 7),
        ('X', 50)
      )
      forAll(testData) { (c: Char, p: Int) => {
        ElfItem(c).priority shouldBe p
      }

      }
    }
  }
}
