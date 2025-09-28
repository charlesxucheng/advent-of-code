package aoc
package aco2022.day4

import common.Utils.loadData

@main def main(): Unit = {
  val filename = "aoc2022-day4-input.txt"
  val data = loadData(filename)(OverlapSectionFinder.parseInput)
  
  println(s"Part 1: ${data.count((x, y) => SectionRange.contains(x, y))}")
  println(s"Part 2: ${data.count((x, y) => x.overlapsWith(y))}")
}
