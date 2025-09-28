package aoc
package aco2022.day4

import aco2022.day3.ElfItem.*

import aoc2022.UnitSpec
import org.scalatest.matchers.should.Matchers.*

class SectionRangeTest extends UnitSpec {
  "SectionRange" should {
    "detect overlaps correctly" in {
      val testData = Table(
        ("Range 1 Start", "Range 1 End", "Range 2 Start", "Range 2 End", "result"),
        (2, 4, 6, 8, false),
        (2, 3, 4, 5, false),
        (5, 7, 7, 9, true),
        (2, 8, 3, 7, true),
        (6, 6, 4, 6, true),
        (2, 6, 4, 8, true),
        (2, 6, 2, 10, true)
      )
      forAll(testData) { (s1, e1, s2, e2, result) =>
        SectionRange(s1, e1).overlapsWith(SectionRange(s2, e2)) shouldBe result
      }
    }
  }
}
