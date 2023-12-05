import cats.implicits.*
import cats.effect.*

object Day4 {

  final case class Card(winningNums: List[Int], actualNums: List[Int]) {
    lazy val totalMatches: Int = winningNums.intersect(actualNums).size

    lazy val points: Int =
      if (totalMatches > 1)
        1 * Math.pow(2.toDouble, (totalMatches - 1).toDouble).toInt
      else if (totalMatches == 1)
        1
      else
        0
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
            .filterNot(_.isEmpty) // for some reason the input had some random extra spaces, so this is needed to clean things up a bit
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

  def part2(): IO[Int] = {

    def totalCardCopies(cards: List[Card]): Int = {

      // Inner recursive function
      def sumCopies(winningCardIndex: Int, cards: List[Card]): Int = {
        val winningCard = cards(winningCardIndex)
        val copies = cards.zipWithIndex.slice(winningCardIndex + 1, winningCardIndex + winningCard.totalMatches + 1)

        copies.foldLeft(0) { case (copiesSum, (card, index)) =>
          if (card.totalMatches > 0)
            copiesSum + 1 + sumCopies(index, cards)
          else
            copiesSum + 1
        }
      }

      cards.zipWithIndex.foldLeft(0) { case (totalSum, (card, index)) =>
        if (card.totalMatches > 0)
          totalSum + 1 + sumCopies(index, cards)
        else
          totalSum + 1
      }
    }

    for {
      _ <- IO.println("Day4 Part 2: calculating...")
      lines <- Input.loadAll[IO]("Day4.txt")
      cards = lines.map(parseIntoCard)
      totalCopies = totalCardCopies(cards)
    } yield totalCopies
  }
}
