import cats.implicits._
import cats.effect._

object Day1 {


  /*
  The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover.
  On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

  For example:

  1abc2
  pqr3stu8vwx
  a1b2c3d4e5f
  treb7uchet
  In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
  */

  // Part 1 Solution
  def sumCoordinates[F[_]](): F[Int] = {

    val file = IO(Source)

  }

}
