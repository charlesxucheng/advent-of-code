package aoc
package aoc2024.day22

import aoc2024.day22.SecretNumber.*
import common.Utils.loadData

object SecretNumber {

  extension (value: Long)
    private inline def mix(n: Long): Long = value ^ n
    private inline def prune: Long = value % 16777216
    private inline def calc(f: Long => Long): Long = mix(f(value)).prune
    inline def nextSecret(): Long = calc(_ << 6).calc(_ >> 5).calc(_ << 11)
}

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Info))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day22-input.txt"
  val input = loadData(filename)(_.toSeq.map(_.toLong))
  scribe.debug(s"$input")

  val result = input.map(List.iterate(_, 2001)(_.nextSecret()))
  scribe.debug(s"$result")

  scribe.info(s"Part 1 Result: ${result.map(_.last).sum}")

  val digits = result.map(_.map(l => l % 10))

  val maps = digits.map(
    _.sliding(5)
      .map { case Seq(n1, n2, n3, n4, n5) =>
        ((n2 - n1, n3 - n2, n4 - n3, n5 - n4), n5)
      }
      .foldLeft(Map.empty[(Long, Long, Long, Long), Long]) {
        case (acc, (key, value)) =>
          acc + (key -> acc.getOrElse(key, value))
      }
  )

  val combinedMap = maps.foldLeft(Map.empty[(Long, Long, Long, Long), Long]) {
    case (acc, map) =>
      map.foldLeft(acc) { case (acc, (key, value)) =>
        acc + (key -> acc.get(key).map(_ + value).getOrElse(value))
      }
  }

  val maxEntry = combinedMap.maxBy { case (_, value) => value }

  println(s"Part 2 Result: $maxEntry")

}
