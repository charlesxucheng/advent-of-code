package aoc
package aco2022.day7

import common.Utils.loadData

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object DirSizeCalculator {
  private val ROOT_DIR_NAME = "/"
  private val DIR_SEPARATOR = "/"

  def parseInput(input: Iterable[String]): Map[String, Directory] = {
    val root = Directory(ROOT_DIR_NAME, Nil, Nil)

    @tailrec
    def parseInputRec(
        input: Iterable[String],
        currentDirName: String,
        directories: Map[String, Directory],
        parentMap: Map[String, String]
    ): Map[String, Directory] = {
      input.headOption match {
        case None => directories
        case Some(line) =>
          fromString(line) match {
            case CdInto(dirName) if dirName == ROOT_DIR_NAME =>
              parseInputRec(input.tail, currentDirName, directories, parentMap)
            case CdUp() =>
              println(
                s"Going back to parent directory ${parentMap(currentDirName)}"
              )
              parseInputRec(
                input.tail,
                directories(parentMap(currentDirName)).name,
                directories,
                parentMap
              )
            case CdInto(dirName) =>
              println(s"Going to directory $dirName")
              parseInputRec(
                input.tail,
                formFullPath(currentDirName, dirName),
                directories,
                parentMap
              )
            case Ls() =>
              println(s"Listing directory $currentDirName")
              parseInputRec(input.tail, currentDirName, directories, parentMap)
            case DirectoryOutput(dirName) =>
              println(
                s"Adding directory $dirName to directory $currentDirName"
              )
              val currentDir = directories(currentDirName)
              val newDirPath = formFullPath(currentDirName, dirName)
              val updatedCurrentDir = currentDir.copy(
                subdirectories = currentDir.subdirectories :+ newDirPath
              )
              parseInputRec(
                input.tail,
                currentDirName,
                directories + (currentDirName -> updatedCurrentDir) + (newDirPath -> Directory(
                  newDirPath,
                  Nil,
                  Nil
                )),
                parentMap + (newDirPath -> currentDirName)
              )
            case FileOutput(fileName, fileSize) =>
              println(
                s"Adding file $fileName to directory $currentDirName"
              )
              val newFile = File(fileName, fileSize)
              val currentDir = directories(currentDirName)
              val updatedCurrentDir = currentDir.copy(
                files = currentDir.files :+ newFile
              )
              parseInputRec(
                input.tail,
                currentDirName,
                directories + (currentDirName -> updatedCurrentDir),
                parentMap
              )
          }
      }
    }

    parseInputRec(
      input,
      root.name,
      HashMap(root.name -> root),
      HashMap(root.name -> root.name)
    )
  }

  private def fromString(line: String): FileSystemIO = {
    line match {
      case s"$$ $cmd $arg" =>
        cmd match {
          case "cd" if arg == ".." => CdUp()
          case "cd"                => CdInto(arg)
        }
      case "$ ls"                 => Ls()
      case s"dir $dirName"        => DirectoryOutput(dirName)
      case s"$fileSize $fileName" => FileOutput(fileName, fileSize.toLong)
    }
  }

  private def formFullPath(parentDirName: String, dirName: String) = {
    if (parentDirName == ROOT_DIR_NAME) parentDirName + dirName
    else parentDirName + DIR_SEPARATOR + dirName
  }

  def calculateDirectorySizes(
      directories: Map[String, Directory]
  ): Map[String, Long] = {

    @tailrec
    def calculateDirectorySizesRec(
        directoryQueue: Iterable[String],
        directorySizes: Map[String, Long]
    ): Map[String, Long] = {
      directoryQueue match {
        case Nil => directorySizes
        case head :: tail =>
          val dir = directories(head)
          val subdirectoryWithUnknownSizes =
            dir.subdirectories.filter(x => !directorySizes.contains(x))

          if (subdirectoryWithUnknownSizes.nonEmpty) {
            calculateDirectorySizesRec(
              subdirectoryWithUnknownSizes ++ directoryQueue,
              directorySizes
            )
          } else { // All subdirectories have known sizes
            val subdirsTotalSize = dir.subdirectories.map(directorySizes(_)).sum
            val filesTotalSize = dir.files.map(_.size).sum
            val newDirectorySizes =
              directorySizes.updated(head, subdirsTotalSize + filesTotalSize)
            calculateDirectorySizesRec(
              tail,
              newDirectorySizes
            )
          }
      }
    }

    calculateDirectorySizesRec(List("/"), HashMap())
  }

  sealed trait FileSystemIO

  case class CdInto(dirName: String) extends FileSystemIO

  case class CdUp() extends FileSystemIO

  case class Ls() extends FileSystemIO

  case class DirectoryOutput(dirName: String) extends FileSystemIO

  case class FileOutput(fileName: String, fileSize: Long) extends FileSystemIO

  case class File(name: String, size: Long)

  case class Directory(
      name: String,
      files: List[File],
      subdirectories: List[String]
  )

}

@main def main(): Unit = {

  val filename = "aoc2022-day7-input.txt"
//  val filename = "test.txt"
  val dirs = loadData(filename)(DirSizeCalculator.parseInput)
  println(dirs)

  val dirSizes = DirSizeCalculator.calculateDirectorySizes(dirs)
  println(dirSizes)

  val part1Result = dirSizes.filter(_._2 <= 100000L)
  println(part1Result)
  println(s"Part 1 result: ${part1Result.values.sum}")

  val totalDiskSpace = 70000000L
  val requiredFreeSpace = 30000000L
  val currentFreeSpace = totalDiskSpace - dirSizes("/")
  val spaceToReclaim = requiredFreeSpace - currentFreeSpace
  val part2Result = dirSizes.filter(_._2 >= spaceToReclaim)
  println(part2Result)
  println(s"Part 2 result: ${part2Result.minBy(_._2)._2}")
}
