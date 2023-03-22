package aoc2022
package common

import org.scalatest.matchers.should.Matchers.should

class CalorieCalculatorTest extends UnitSpec {
    "CalorieCalculator" should {
        "parse nil input string correctly" in {
            val input = ""
            val expected = List.empty
            CalorieCalculator.parseInput(input) should be (expected)
        }

        "parse single input string correctly" in {
            val input = "1"
            val expected = List(List(1))
            CalorieCalculator.parseInput(input) should be (expected)
        }

        "parse one group of multiple input values correctly" in {
            val input = "1;2;3"
            val expected = List(List(1, 2, 3))
            CalorieCalculator.parseInput(input) should be (expected)
        }

        "parse multiple groups of input values correctly" in {
            val input = "1;2;3;;1;1;1"
            val expected = List(List(1, 2, 3), List(1, 1, 1))
            CalorieCalculator.parseInput(input) should be(expected)
        }

        "parse inputs with extra blank lines correctly" in {
            val input = "1;2;3;;1;1;1;;"
            val expected = List(List(1, 2, 3), List(1, 1, 1))
            CalorieCalculator.parseInput(input) should be(expected)
        }

        "find max value for nil input" in {
            val input = List.empty[List[Int]]
            assertThrows[IllegalArgumentException] {
                CalorieCalculator.findMax(input)
            }
        }

        "find max value for 1 group of values" in {
            val input = List(List(1, 2, 3))
            val expected = 6
            CalorieCalculator.findMax(input) should be(expected)
        }

        "find max value for 2 groups of values" in {
            val input = List(List(1, 2, 3), List(5, 6, 2))
            val expected = 13
            CalorieCalculator.findMax(input) should be(expected)
        }
    }
}
