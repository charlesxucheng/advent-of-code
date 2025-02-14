package aoc
package aoc2024.day23

import common.Utils.loadData

type Node = String
type LanGraph = Map[Node, Set[Node]]

object LanParty {

  def parseInput(input: Iterator[Node]): LanGraph =
    input.toSet
      .flatMap { case s"$a-$b" => Set(a -> b, b -> a) }
      .groupMap(_._1)(_._2)

  def isValidTriple(graph: LanGraph, nodes: Set[Node]): Boolean = {
    assert(nodes.size == 3)

    val (a, b, c) = nodes.toList match {
      case List(a, b, c) => (a, b, c)
    }

    isConnected(graph, a, b) && isConnected(graph, b, c) && isConnected(graph, a, c)
  }

  private def isConnected(graph: LanGraph, a: Node, b: Node): Boolean =
    graph.getOrElse(a, Set.empty).contains(b) || graph
      .getOrElse(b, Set.empty)
      .contains(a)

}

@main
def main(): Unit = {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(scribe.Level.Info))
    .replace()

//  val filename = "test.txt"
  val filename = "aoc2024-day23-input.txt"
  val input = loadData(filename)(LanParty.parseInput)
  scribe.debug(s"$input")

  val triples = input.flatMap { (node, neighbors) =>
    neighbors
      .subsets(2)
      .map(_ + node)
      .withFilter(a => a.exists(_.startsWith("t")))
      .withFilter(LanParty.isValidTriple(input, _))
  }.toSet

  scribe.debug(s"$triples")

  scribe.info(s"Part 1 result: ${triples.size}")

}
