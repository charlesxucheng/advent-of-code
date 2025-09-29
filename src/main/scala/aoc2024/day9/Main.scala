package aoc
package aoc2024.day9

import aoc2024.day9.FileSystem.*
import common.Utils.loadData
import scala.annotation.tailrec

sealed trait FileSystemObject() {
  val size: Int
}

case class File(id: Long, size: Int) extends FileSystemObject
case class FreeSpace(size: Int) extends FileSystemObject

object FileSystem {
  def parseInput(input: Iterable[String]): Seq[FileSystemObject] =
    input.iterator.next().toSeq.zipWithIndex.map { case (char, id) =>
      if (id % 2 == 0) File(id / 2, Integer.parseInt(char.toString))
      else FreeSpace(Integer.parseInt(char.toString))
    }

  def compact(fsObjects: Seq[FileSystemObject]): Seq[FileSystemObject] =
    compactRec(fsObjects.tail, List(fsObjects.head))

  @tailrec
  private def compactRec(
      fsObjects: Seq[FileSystemObject], // Reversed
      acc: Seq[FileSystemObject]
  ): Seq[FileSystemObject] =
    fsObjects match {
      case Vector() => acc
      case x +: xs =>
        x match {
          case file: File => compactRec(xs, acc :+ file)
          case freeSpace: FreeSpace =>
            xs match {
              case Vector() => acc :+ freeSpace
              case _ =>
                xs.last match {
                  case FreeSpace(_) => compactRec(fsObjects.dropRight(1), acc)
                  case File(id, fileSize) =>
                    if (fileSize == freeSpace.size)
                      compactRec(
                        xs.dropRight(1),
                        acc :+ File(id, freeSpace.size)
                      )
                    else if (fileSize < freeSpace.size)
                      compactRec(
                        FreeSpace(freeSpace.size - fileSize) +: xs.dropRight(1),
                        acc :+ File(id, fileSize)
                      )
                    else
                      compactRec(
                        xs.dropRight(1) :+ File(id, fileSize - freeSpace.size),
                        acc :+ File(id, freeSpace.size)
                      )
                }

            }
        }
    }

  def compactWholeFile(
      fsObjects: Seq[FileSystemObject]
  ): Seq[FileSystemObject] =
    compactWholeFileRec(fsObjects, fsObjects.size - 1)

  @tailrec
  private def compactWholeFileRec(
      fsObjects: Seq[FileSystemObject],
      index: Int
  ): Seq[FileSystemObject] = {
//    print("Compacting ")
//    printFileSystemObjects(fsObjects)
    if (index == 0) fsObjects
    else {
      val x = fsObjects(index)
      x match {
        case _: FreeSpace => compactWholeFileRec(fsObjects, index - 1)
        case file: File =>
          val moveToIndex = fsObjects.indexWhere {
            case fs: FreeSpace => fs.size >= file.size
            case _: File => false
          }

          // The file can fit into some free space before it
          if (
            moveToIndex >= 0 && moveToIndex < fsObjects.indexWhere(_ == file)
          ) {
            val resultFsObjects =
              moveFileToFreeSpace(fsObjects, index, moveToIndex)
            compactWholeFileRec(
              resultFsObjects,
              Math.min(index + 1, resultFsObjects.size - 1)
            )
          }
          // The file cannot fit into any free space before it - move on to next element
          else
            compactWholeFileRec(fsObjects, index - 1)
      }
    }
  }

  private def moveFileToFreeSpace(
      seq: Seq[FileSystemObject],
      fromIndex: Int,
      toIndex: Int
  ): Seq[FileSystemObject] = {
    require(
      fromIndex >= 0 && fromIndex < seq.length,
      "fromIndex is out of bounds"
    )
    require(toIndex >= 0 && toIndex < seq.length, "toIndex is out of bounds")

    seq(fromIndex) match {
      case _: FreeSpace =>
        throw IllegalArgumentException(
          "Source of compact cannot be a FreeSpace"
        )
      case fileToMove: File =>
        seq(toIndex) match {
          case _: File =>
            throw IllegalArgumentException(
              "Destination of compact cannot be a File"
            )
          case freeSpace: FreeSpace =>
            val (before, after) = seq.splitAt(fromIndex)
            val seqWithoutFileToMove =
              before :+ FreeSpace(fileToMove.size) :++ after.tail
            val (beforeNew, afterNew) = seqWithoutFileToMove.splitAt(toIndex)

            val objectsAtToIndex =
              if (freeSpace.size > fileToMove.size)
                Seq(
                  File(fileToMove.id, fileToMove.size),
                  FreeSpace(freeSpace.size - fileToMove.size)
                )
              else
                Seq(File(fileToMove.id, freeSpace.size))

            beforeNew ++ objectsAtToIndex ++ afterNew.tail
        }
    }
  }

  def printFileSystemObjects(fsObjects: Seq[FileSystemObject]): Unit = {
    fsObjects.foreach {
      case file: File           => for (i <- 0 until file.size) print(file.id)
      case freeSpace: FreeSpace => for (i <- 0 until freeSpace.size) print(".")
    }
    println()
  }

  def computeCheckSum(fsObjects: Seq[FileSystemObject]): Long =
    fsObjects
      .flatMap {
        case file: File => (0 until file.size).map(_ => file.id)
        case freeSpace: FreeSpace => (0 until freeSpace.size).map(_ => 0L)
      }
      .zipWithIndex
      .map { case (id, index) => id * index }
      .sum
}

@main def main(): Unit = {

  val filename = "aoc2024-day9-input.txt"
//  val filename = "test.txt"
  val filesystemObjects = loadData(filename)(parseInput)
//  printFileSystemObjects(filesystemObjects)

  // Part 1
  val compacted = compact(filesystemObjects)
//  println(s"Compacted: $compacted")
//  printFileSystemObjects(compacted)
  println(computeCheckSum(compacted))

  // Part 2
  val compactedPart2 = compactWholeFile(filesystemObjects)
//  println(s"Compacted: $compactedPart2")
//  printFileSystemObjects(compactedPart2)
  println(computeCheckSum(compactedPart2))
}
