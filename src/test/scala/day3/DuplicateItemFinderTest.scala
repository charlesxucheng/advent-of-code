package aoc2022
package day3

import ElfItem.ElfItem
import org.scalatest.matchers.should.Matchers.shouldBe

class DuplicateItemFinderTest extends UnitSpec {

  private val testData = Table(
    ("input", "items", "priority"),
    ("AbccDa", Set(ElfItem('c')), 3),
    ("YfzHaaaXfGbLkx", Set(ElfItem('f')), 6),
    ("XX", Set(ElfItem('X')), 50),
    ("AbccEAce", Set(ElfItem('A'), ElfItem('c')), 30),
    ("dddd", Set(ElfItem('d')), 4),
    ("AXFPPFXA", Set(ElfItem('A'), ElfItem('X'), ElfItem('F'), ElfItem('P')), 27 + 50 + 32 + 42)
  )

  "DuplicateItemFinder" when {
    "given two empty strings" should {
      "find nothing" in {
        DuplicateItemFinder.findCommonItems(List.empty) shouldBe Set.empty
      }
    }
    "given two strings with common characters" should {
      "find the common characters" in {
        forAll(testData) { (input: String, result: Set[ElfItem], _: Any) =>
          DuplicateItemFinder.findCommonItems(input) shouldBe result
        }
      }
    }
    "given two empty strings" should {
      "calculate its priority as 0" in {
        DuplicateItemFinder.sumPriority(DuplicateItemFinder.findCommonItems(List.empty)) shouldBe 0
      }
    }
    "given two strings representing ElfItems" should {
      "sum their priority correctly" in {
        forAll(testData) { (s: String, _: Set[ElfItem], p: Int) => {
          DuplicateItemFinder.sumPriority(DuplicateItemFinder.findCommonItems(s)) shouldBe p
        }}
      }
    }
    "given a list of strings representing ElfItems" should {
      "sum their priorities correctly" in {
        val testData = Seq("AbccDa", "YfzHaaaXfGbLkx", "XX", "AbccEAce", "dddd", "AXFPPFXA").iterator
        val elfItems = DuplicateItemFinder.parseInput(testData)
        DuplicateItemFinder.sumAllPriority(elfItems) shouldBe 244
      }
    }
  }
}
