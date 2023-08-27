package aoc2022
package day3

import ElfItem.ElfItem
import org.scalatest.matchers.should.Matchers.shouldBe

class DuplicateItemFinderTest extends UnitSpec {
  "DuplicateItemFinder" when {
    "given two empty strings" should {
      "find nothing" in {
        DuplicateItemFinder.findCommonItems(List.empty) shouldBe Set.empty
      }
    }
    "given two strings with exact one common character" should {
      "find one common character" in {
        val testData = Table(
          ("input", "result"),
          ("AbccDa", Set(ElfItem('c'))),
          ("YfzHaaaXfGbLkx", Set(ElfItem('f'))),
          ("XX", Set(ElfItem('X')))
        )

        forAll(testData) { (input: String, result: Set[ElfItem]) =>
          DuplicateItemFinder.findCommonItems(input) shouldBe result
        }
      }
    }

  }
}
