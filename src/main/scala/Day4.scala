import cats.implicits.*
import cats.effect.*

object Day4 {

  final case class Card(winningNums: List[Int], actualNums: List[Int]) {
    lazy val points: Int = {
      val totalMatches = winningNums.intersect(actualNums).size
      if (totalMatches > 1)
        1 * Math.pow(2.toDouble, (totalMatches - 1).toDouble).toInt
      else if (totalMatches == 1)
        1
      else
        0
    }
  }

  def parseIntoCard(line: String): Card = {
    /*
    Example:
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
     */
    line.split('|').toList match {
      case idAndWinningNumbers :: actualNumberString :: Nil =>
        val winningNumbers =
          idAndWinningNumbers
            .split(":")
            .tail.head // get section with actual numbers
            .trim
            .split(" ")
            .toList
            .filterNot(_.isEmpty)
            .map(_.toInt)

        val actualNumbers = actualNumberString.trim.split(" ").toList.filterNot(_.isEmpty).map(_.toInt)

        Card(winningNumbers, actualNumbers)
    }
  }

  def part1(): IO[Int] = {

    for {
      _ <- IO.println("Day4 Part 1: calculating...")
      lines <- Input.loadAll[IO]("Day4.txt")
      cards = lines.map(parseIntoCard)
      points = cards.map(_.points)
      _ <- IO.println(s"Cards: $cards")
    } yield points.sumAll
  }
}
