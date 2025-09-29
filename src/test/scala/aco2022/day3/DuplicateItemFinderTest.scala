package aoc
package aco2022.day3

import aco2022.day3.DuplicateItemFinder
import aco2022.day3.ElfItem.ElfItem

import aoc2022.UnitSpec
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
        val testData = Seq("AbccDa", "YfzHaaaXfGbLkx", "XX", "AbccEAce", "dddd", "AXFPPFXA")
        val elfItems = DuplicateItemFinder.parseInput(testData)
        DuplicateItemFinder.sumAllPriority(elfItems) shouldBe 244
      }
    }
    "given a list of strings representing ElfItems" should {
      "parse them into a list of list of ElfItems" in {
        val testData = Seq("AbccDa", "XX")
        val elfItems = DuplicateItemFinder.parseInput(testData)
        elfItems shouldBe List(
          List(ElfItem('A'), ElfItem('b'), ElfItem('c'), ElfItem('c'), ElfItem('D'), ElfItem('a')),
          List(ElfItem('X'), ElfItem('X'))
        )
      }
    }
    "given a list of three strings representing ElfItems in Part 2" should {
      "parse them into three lists of ElfItems" in {
        val testData = Seq("vJrwpv", "jqHR", "Pmmdzm")
        val elfItems = DuplicateItemFinder.parseInput2(testData)
        elfItems shouldBe List(
          (List(ElfItem('v'), ElfItem('J'), ElfItem('r'), ElfItem('w'), ElfItem('p'), ElfItem('v')),
            List(ElfItem('j'), ElfItem('q'), ElfItem('H'), ElfItem('R')),
            List(ElfItem('P'), ElfItem('m'), ElfItem('m'), ElfItem('d'), ElfItem('z'), ElfItem('m'))
          )
        )
      }
    }
    "given a list of strings representing ElfItems with badges in Part 2" should {
      "calculate the sum of the priority of all the badges" in {
        val testData = Seq(
          "vJrwpWtwJgWrhcsFMMfFFhFp",
          "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
          "PmmdzqPrVvPwwTWBwg",
          "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
          "ttgJtRGJQctTZtZT",
          "CrZsJsPPZsGzwwsLwLmpwMDw"
        )
        val elfItems = DuplicateItemFinder.parseInput2(testData)
        val badgePriority = DuplicateItemFinder.sumAllBadgePriority(elfItems)
        badgePriority shouldBe ElfItem('r').priority + ElfItem('Z').priority
      }
    }
  }
}
