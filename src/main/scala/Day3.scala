import cats.implicits._
import cats.effect._

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
  def part1(): IO[Int] = {

    /*
    Approach

    Given a number character on a line, how can we tell if it's adjacent to a symbol? Check all the characters surrounding it, including on the previous line and next line.
     */

    def isAdjacentToSymbol(
      currentLine: String,
      index: Int,
      maybeLineAbove: Option[String] = None,
      maybeLineBelow: Option[String] = None
    ): Boolean = {

      def hasSymbolAtIndex(string: String, index: Int): Boolean =
        if (index < 0 || index >= string.length) false // out of bounds
        else
          val char = string.charAt(index)
          (char != '.') && !char.isDigit

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

    def getAllPartNumbers(lines: List[String]): List[Int] = {


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

      def getPartNumbersFromLineGroup(
         maybeLineAbove: Option[String],
         currentLine: String,
         maybeLineBelow: Option[String]
       ): List[Int] = {
        getPartNumbersFromLine(currentLine).filter { partNumber =>
          partNumber.indices.exists(index => isAdjacentToSymbol(currentLine, index, maybeLineAbove, maybeLineBelow))
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

  def part2(): IO[Int] = {

    for {
      lines <- Input.loadAll[IO]("Day3.txt")
      result = 2
    } yield result
  }
}
