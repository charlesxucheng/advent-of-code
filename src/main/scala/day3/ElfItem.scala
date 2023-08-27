package aoc2022
package day3

object ElfItem {
  opaque type ElfItem = Char

  object ElfItem {
    def apply(char: Char): ElfItem = {
      require(char.<=('z') && char.>=('a') || char.<=('Z') && char.>=('A'))
      char
    }

    def from(string: String): List[ElfItem] = string.toList.map(ElfItem(_))
  }

  extension(elfItem: ElfItem)
    def priority: Int = elfItem.toInt - 'a'.toInt + 1
    def toChar: Char = elfItem
}
