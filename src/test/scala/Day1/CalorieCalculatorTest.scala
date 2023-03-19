package aoc2022
package Day1

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
            val expected = List(1)
            CalorieCalculator.parseInput(input) should be (expected)
        }

        "parse one group of multiple input values correctly" in {
            val input = "1;2;3"
            val expected = List(6)
            CalorieCalculator.parseInput(input) should be (expected)
        }

        "parse multiple groups of input values correctly" in {
            val input = "1;2;3;;1;1;1"
            val expected = List(6, 3)
            CalorieCalculator.parseInput(input) should be(expected)
        }

        "parse inputs with extra blank lines correctly" in {
            val input = "1;2;3;;1;1;1;;"
            val expected = List(6, 3)
            CalorieCalculator.parseInput(input) should be(expected)
        }

    }
}
