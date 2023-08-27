package aoc2022
package day3

object ElfItem {
  opaque type ElfItem = Char

  object ElfItem {
    def apply(char: Char): ElfItem = {
      require( isSmallLetter(char)|| isCapitalLetter(char))
      char
    }

    def isSmallLetter(char: Char): Boolean = char.<=('z') && char.>=('a')
    def isCapitalLetter(char: Char): Boolean = char.<=('Z') && char.>=('A')

    def from(string: String): List[ElfItem] = string.toList.map(ElfItem(_))
  }

  extension(elfItem: ElfItem)
    def priority: Int =
      if (ElfItem.isSmallLetter(elfItem)) elfItem.toInt - 96
      else elfItem.toInt - 38
    def toChar: Char = elfItem
}
