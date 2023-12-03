import cats.implicits._
import cats.effect._

object Day1 {

  // Example solution shown below

  /**
   * Get the first and last digit from each line, then sum all values together
   * @return
   */
  def sumCoordinates(): IO[Int] =
    for {
      lines <- Input.loadAll("Day1.txt")
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
}
