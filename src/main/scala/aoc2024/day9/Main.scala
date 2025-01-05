package aoc
package aoc2024.day9

import aoc2024.day9.FileSystem.{compact, computeCheckSum, parseInput}
import common.Utils.loadData
import scala.annotation.tailrec

sealed trait FileSystemObject() {
  val size: Int
}

case class File(id: Long, size: Int) extends FileSystemObject
case class FreeSpace(size: Int) extends FileSystemObject

object FileSystem {
  def parseInput(input: Iterator[String]): List[FileSystemObject] =
    input.next().toList.zipWithIndex.map { case (char, id) =>
      if (id % 2 == 0) File(id / 2, Integer.parseInt(char.toString))
      else FreeSpace(Integer.parseInt(char.toString))
    }

  def compact(fsObjects: List[FileSystemObject]): List[FileSystemObject] =
    compactRec(fsObjects.tail, List(fsObjects.head))

  @tailrec
  private def compactRec(
      fsObjects: List[FileSystemObject],
      acc: List[FileSystemObject]
  ): List[FileSystemObject] = fsObjects match {
    case Nil => acc
    case x :: xs =>
      x match {
        case file: File => compactRec(xs, acc :+ file)
        case freeSpace: FreeSpace =>
          xs match {
            case Nil => acc :+ freeSpace
            case ys :+ y =>
              y match {
                case FreeSpace(_) => compactRec(x :: ys, acc)
                case File(id, fileSize) =>
                  if (fileSize == freeSpace.size)
                    compactRec(ys, acc :+ File(id, freeSpace.size))
                  else if (fileSize < freeSpace.size)
                    compactRec(
                      FreeSpace(freeSpace.size - fileSize) :: ys,
                      acc :+ File(id, fileSize)
                    )
                  else
                    compactRec(
                      ys :+ File(id, fileSize - freeSpace.size),
                      acc :+ File(id, freeSpace.size)
                    )
              }
            case head :: tail =>
              println(s"Something is wrong: $head $tail")
              compactRec(xs, acc)
          }
      }
  }

  def printFileSystemObjects(fsObjects: List[FileSystemObject]): Unit = {
    fsObjects.foreach {
      case file: File           => for (i <- 0 until file.size) print(file.id)
      case freeSpace: FreeSpace => for (i <- 0 until freeSpace.size) print(".")
    }
    println()
  }

  def computeCheckSum(fsObjects: List[FileSystemObject]): Long =
    fsObjects
      .collect { case file: File => file }
      .flatMap(file => (0 until file.size).map(_ => file.id))
      .zipWithIndex
      .map { case (id, index) => id * index }
      .sum
}

@main def main(): Unit = {

  val filename = "aoc2024-day9-input.txt"
//  val filename = "test.txt"
  val filesystemObjects = loadData(filename)(parseInput)
//  println(filesystemObjects)

  // Part 1
  val compacted = compact(filesystemObjects)
//  println(s"Compacted: $compacted")
//  printFileSystemObjects(compacted)
  println(computeCheckSum(compacted))
}
