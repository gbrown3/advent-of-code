import cats.implicits._
import cats.effect._

object Day1 {


  /*
  Part 1

  The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover.
  On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

  For example:

  1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet
  In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
  */
  def sumCoordinates(): IO[Int] =
    for {
      lines <- Input.loadAll[IO]("Day1.txt")
      digits <- {
        lines.map { line =>
          def getFirstDigit(string: String): IO[Char] = {
            val maybeChar = string.collectFirst {
              case numChar if numChar.isDigit => numChar
            }
            IO.fromOption(maybeChar)(new RuntimeException(s"couldn't find a digit in string: $string"))
          }

          for {
            firstDigit <- getFirstDigit(line)
            secondDigit <- getFirstDigit(line.reverse)
          } yield s"$firstDigit$secondDigit".toInt
        }.sequence
      }
    } yield digits.sumAll

    // Expected result: 53974

  /*
  Part 2

  Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

  Equipped with this new information, you now need to find the real first and last digit on each line. For example:

  two1nine
  eightwothree
  abcone2threexyz
  xtwone3four
  4nineeightseven2
  zoneight234
  7pqrstsixteen
  In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
   */

  /*
  Approach

  Finding the first digit:

  Loop through characters in line from left to right.
  As you loop, keep track of the current word that could be a word digit.
  This should initialize to empty string "".
    Is the character a digit?
        Yes -> stop searching. Digit found.
        No ->
          Append character to the current word
          Does the current word match any known word digits?
            Yes -> stop searching. Digit found
            No ->
              Are any word digits a substring of the current word?
                Yes -> stop searching, digit found
                No -> keep searching, move on to next character

   Finding the second digit:

   Follow the same algorithm as the first digit, with two notable exceptions:
   1. Loop through characters from right to left
   2. When determining the current word, prepend the next character rather than appending
   */
  def sumCoordinatesAdvanced(): IO[Int] = {

    val wordDigits = Map(
      "one" -> '1',
      "two" -> '2',
      "three" -> '3',
      "four" -> '4',
      "five" -> '5',
      "six" -> '6',
      "seven" -> '7',
      "eight" -> '8',
      "nine" -> '9'
    )

    def containsDigit(string: String): Option[Char] =
      wordDigits.keySet.foldLeft[Option[Char]](None) { case (maybeFoundDigit, wordDigit) =>
        if (maybeFoundDigit.isDefined) maybeFoundDigit
        else if (string.contains(wordDigit)) Some(wordDigits(wordDigit))
        else None
      }

    for {
      lines <- Input.loadAll("Day1.txt")
      digits <- {
        lines.map { line =>
          final case class SearchResult(word: String = "", foundDigit: Option[Char] = None)

          def digitSearch(searchResult: SearchResult, nextChar: Char)(reverse: Boolean): SearchResult = {
            // Digit already found - no processing needed.
            if (searchResult.foundDigit.isDefined) searchResult

            // This next character is a digit all on it's own!
            else if (nextChar.isDigit) searchResult.copy(foundDigit = Some(nextChar))

            // Time to see if this is a word digit...
            else {
              val currentWord =
                if (reverse) nextChar + searchResult.word
                else searchResult.word + nextChar

              // We've found a complete word digit. Stop the search
              if (wordDigits.keySet.contains(currentWord)) {
                searchResult.copy(foundDigit = Some(wordDigits(currentWord)))
              }

              containsDigit(currentWord) match {
                // We've stumbled into a complete word digit. Stop the search
                case Some(digit) => searchResult.copy(foundDigit = Some(digit))
                // We haven't figured it out yet...
                case None => searchResult.copy(word = currentWord)
              }
            }
          }

          def digitSearchReverse(nextChar: Char, searchResult: SearchResult): SearchResult = digitSearch(searchResult, nextChar)(reverse = true)

          def getFirstDigit(string: String): IO[Char] = {
            val searchResult = string.toCharArray.toList.foldLeft(SearchResult())(digitSearch(_, _)(reverse = false))
            IO.fromOption(searchResult.foundDigit)(new RuntimeException(s"could not find any digits in string: $string"))
          }

          def getSecondDigit(string: String): IO[Char] = {
            val searchResult = string.toCharArray.toList.foldRight(SearchResult())(digitSearchReverse)
            IO.fromOption(searchResult.foundDigit)(new RuntimeException(s"could not find any digits in string: $string"))
          }

          val result = for {
            firstDigit <- getFirstDigit(line)
            secondDigit <- getSecondDigit(line)
            result = s"$firstDigit$secondDigit".toInt
            _ <- IO.println(s"line: $line")
            _ <- IO.println(s"result: $result")
          } yield result

          result
        }.sequence
      }
    } yield digits.sumAll

    // Expected: 52840
  }
}
