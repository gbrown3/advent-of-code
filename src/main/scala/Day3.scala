import cats.implicits._
import cats.effect._

import scala.language.postfixOps

object Day3 {

  /*
  The engine schematic (your puzzle input) consists of a visual representation of the engine.
  There are lots of numbers and symbols you don't really understand, but apparently
  any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum.
  (Periods (.) do not count as a symbol.)

  Here is an example engine schematic:

  467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..
  In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right).
  Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

  Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?
   */


  def isAdjacentToSymbol(
    currentLine: String,
    index: Int,
    maybeLineAbove: Option[String] = None,
    maybeLineBelow: Option[String] = None
  )(validSymbol: Char => Boolean): Boolean = {

    def hasSymbolAtIndex(string: String, index: Int): Boolean =
      if (index < 0 || index >= string.length) false // out of bounds
      else {
        val char = string.charAt(index)
        validSymbol(char)
      }

    // Check the current line. Are the characters to the left and right of this character symbols?
    val leftIsSymbol = hasSymbolAtIndex(currentLine, index - 1)
    val rightIsSymbol = hasSymbolAtIndex(currentLine, index + 1)

    // Check the line above, if one exists. Are any of the characters with the same index +/- 1 symbols?
    val lineAboveHasSymbol = maybeLineAbove.fold(false) { lineAbove =>
      hasSymbolAtIndex(lineAbove, index - 1) ||
        hasSymbolAtIndex(lineAbove, index) ||
        hasSymbolAtIndex(lineAbove, index + 1)
    }

    // Check the line below, if one exists. Are any of the characters with the same index +/- 1 symbols?
    val lineBelowHasSymbol = maybeLineBelow.fold(false) { lineAbove =>
      hasSymbolAtIndex(lineAbove, index - 1) ||
        hasSymbolAtIndex(lineAbove, index) ||
        hasSymbolAtIndex(lineAbove, index + 1)
    }

    leftIsSymbol || rightIsSymbol || lineAboveHasSymbol || lineBelowHasSymbol
  }

  /*
  Current approach assumes that the part numbers are individual digits, not full numbers.
  We need to check if the full numbers are adjacent, then add THOSE numbers together.

  New strat:
      Scan through current line and figure out what the part numbers are.
      Should be easy, anything that starts with a digit up until you hit a non-digit.
      Keep track of all indices for a given part number.
      Then check all indices as normal - if any are adjacent to a symbol, the part should be included in the list.
  */

  final case class PartNumber(number: Int, indices: List[Int])

  def getPartNumbersFromLine(line: String): List[PartNumber] = {
    final case class LoopData(
       partNumbers: List[PartNumber] = List.empty,
       currentNumberString: String = "",
       currentIndices: List[Int] = List.empty
     )

    line.zipWithIndex.foldLeft(LoopData()) { case (loopData, (nextChar, index)) =>
      if (nextChar.isDigit) {
        // If we're about to hit the end of the line, save the current number
        if (index == line.length - 1) {
          val updatedNumber = loopData.currentNumberString + nextChar
          val newPartNumber = PartNumber(updatedNumber.toInt, loopData.currentIndices :+ index)

          loopData.copy(partNumbers = loopData.partNumbers :+ newPartNumber)
        }
        // Otherwise, keep building out the current part number
        else {
          loopData.copy(
            currentNumberString = loopData.currentNumberString + nextChar,
            currentIndices = loopData.currentIndices :+ index
          )
        }
      }
      else {
        // We've hit the end of a part number
        if (loopData.currentNumberString.nonEmpty) {
          val newPartNumber = PartNumber(loopData.currentNumberString.toInt, loopData.currentIndices)
          // Add new partner number and reset other loop data
          LoopData(loopData.partNumbers :+ newPartNumber)
        } else {
          // We hit some dummy data, ignore it
          loopData
        }
      }
    }.partNumbers
  }

  def part1(): IO[Int] = {

    /*
    Approach

    Given a number character on a line, how can we tell if it's adjacent to a symbol? Check all the characters surrounding it, including on the previous line and next line.
     */

    def getAllPartNumbers(lines: List[String]): List[Int] = {

      def getPartNumbersFromLineGroup(
         maybeLineAbove: Option[String],
         currentLine: String,
         maybeLineBelow: Option[String]
       ): List[Int] = {
        getPartNumbersFromLine(currentLine).filter { partNumber =>
          partNumber.indices.exists(index => isAdjacentToSymbol(currentLine, index, maybeLineAbove, maybeLineBelow)(char => (char != '.') && !char.isDigit))
        }.map(_.number)
      }

      // NOTE - this doesn't account for first and last lines
      val middlePartNumbers = lines.sliding(3).toList.foldLeft(List.empty[Int]) {
        case (partNumbers, lineGroup) =>
          lineGroup match {
            // Standard group of three
            case lineAbove :: currentLine :: lineBelow :: Nil =>
              val newPartNumbers = getPartNumbersFromLineGroup(Some(lineAbove), currentLine, Some(lineBelow))
              partNumbers ++ newPartNumbers
          }
      }

      val firstLinePartNumbers = getPartNumbersFromLineGroup(maybeLineAbove = None, lines.head, lines.tail.headOption)
      val lastLinePartNumbers = getPartNumbersFromLineGroup(Some(lines(lines.length - 2)), lines.last, None)

      firstLinePartNumbers ++ middlePartNumbers ++ lastLinePartNumbers
    }

    for {
      lines <- Input.loadAll[IO]("Day3.txt")
      partNumbers = getAllPartNumbers(lines)
    } yield partNumbers.sumAll
  }

  /*
  The missing part wasn't the only issue - one of the gears in the engine is wrong.
  A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.

  This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.
   */
  def part2(): IO[Int] = {

    /*
    Approach:

    1. Get all the part numbers, and their indices
    2. Scan in groups of three, check if there are any gear ratios
      a. How to determine if there's a gear ratio
        i. Look for *'s. Check how many part numbers are adjacent.
        ii. If exactly two parts are adjacent, we've found a gear ratio
     */

    def getGearRatios(lines: List[String]): List[Int] = {

      def getGearRatiosFromLineGroup(
         maybeLineAbove: Option[String],
         currentLine: String,
         maybeLineBelow: Option[String]
       ): List[Int] = {
        val currentLinePartNumbers: List[PartNumber] = getPartNumbersFromLine(currentLine)
        val lineAbovePartNumbers: List[PartNumber] = maybeLineAbove.fold(List.empty[PartNumber])(getPartNumbersFromLine)
        val lineBelowPartNumbers: List[PartNumber] = maybeLineBelow.fold(List.empty[PartNumber])(getPartNumbersFromLine)

        def buildPartIndexMap(parts: List[PartNumber]): Map[Int, List[PartNumber]] = {
          parts.foldLeft(Map.empty[Int, List[PartNumber]]) { case (indexMap, part) =>
            part.indices.foldLeft(indexMap) { case (map, index) =>
              if (map.contains(index)) map.updated(index, map(index) :+ part)
              else map.updated(index, List(part))
            }
          }
        }

        val partsByIndex: Map[Int, List[PartNumber]] =
          buildPartIndexMap(currentLinePartNumbers ++ lineAbovePartNumbers ++ lineBelowPartNumbers)


        currentLine.zipWithIndex.foldLeft(List.empty[Int]) { case (gearRatios, (char, index)) =>
          if (char == '*') {
            // Star found. Look around to see if it has gear ratios.

            val adjacentParts: List[PartNumber] =
              partsByIndex.get(index).orEmpty ++
                partsByIndex.get(index - 1).orEmpty ++
                partsByIndex.get(index + 1).orEmpty


            adjacentParts.toSet.toList match {
              case part1 :: part2 :: Nil =>

                gearRatios :+ (part1.number * part2.number)
              case _ =>
                gearRatios
            }
          }
          else
            gearRatios
        }
      }

      // NOTE - this doesn't account for first and last lines
      val middlePartNumbers = lines.sliding(3).toList.foldLeft(List.empty[Int]) {
        case (partNumbers, lineGroup) =>
          lineGroup match {
            // Standard group of three
            case lineAbove :: currentLine :: lineBelow :: Nil =>
              val newPartNumbers = getGearRatiosFromLineGroup(Some(lineAbove), currentLine, Some(lineBelow))
              partNumbers ++ newPartNumbers
          }
      }

      val firstLinePartNumbers = getGearRatiosFromLineGroup(maybeLineAbove = None, lines.head, lines.tail.headOption)
      val lastLinePartNumbers = getGearRatiosFromLineGroup(Some(lines(lines.length - 2)), lines.last, None)

      firstLinePartNumbers ++ middlePartNumbers ++ lastLinePartNumbers
    }


    for {
      _ <- IO.println("Day 3 Part 2: calculating...")
      lines <- Input.loadAll[IO]("Day3.txt")
      gearRatios = getGearRatios(lines)
    } yield gearRatios.sumAll
  }
}
