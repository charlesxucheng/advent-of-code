package aoc
package aoc2024.day25

import common.Utils.loadData

object Schematics {
  private def toHeights(input: Array[Array[Char]]): IndexedSeq[Int] =
    input.head.indices.map(col => input.map(row => row(col)).count(_ == '#'))

  def parseInput(
      input: Iterator[String]
  ): (Seq[IndexedSeq[Int]], Seq[IndexedSeq[Int]]) = {
    val (locksRaw, keysRaw) =
      input
        .grouped(8)
        .toSeq
        .map(_.filter(_.nonEmpty))
        .map(_.map(_.toCharArray).toArray)
        .partition(_.head.forall(_ == '#'))

    val locks = locksRaw.map(toHeights)
    val keys = keysRaw.map(toHeights)
    (locks, keys)
  }
}

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Info))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day25-input.txt"
  val input = loadData(filename)(Schematics.parseInput)
  scribe.debug(s"$input")

  val matches = for
    lock <- input._1
    key <- input._2
    if lock.zip(key).forall({ case (l, k) => l + k <= 7 })
  yield (lock, key)

  scribe.info(s"Part 1 result: ${matches.size}")
}
