import cats.implicits._
import cats.effect._
import enumeratum._
import enumeratum.EnumEntry._
import enumeratum.values.{IntEnum, IntEnumEntry}

object Day7 {

  sealed abstract class CardType(override val entryName: String) extends EnumEntry
  object CardType extends Enum[CardType] {
    val values = findValues

    // A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2
    case object A extends CardType("A")
    case object K extends CardType("K")
    case object Q extends CardType("Q")
    case object J extends CardType("J")
    case object T extends CardType("T")
    case object Nine extends CardType("9")
    case object Eight extends CardType("8")
    case object Seven extends CardType("7")
    case object Six extends CardType("6")
    case object Five extends CardType("5")
    case object Four extends CardType("4")
    case object Three extends CardType("3")
    case object Two extends CardType("2")
    case object One extends CardType("1")

    private def toInt(cardType: CardType): Int = cardType match {
      case A => 14
      case K => 13
      case Q => 12
      case J => 11
      case T => 10
      case number => number.entryName.toInt
    }

    private def toIntWithJokers(cardType: CardType): Int = cardType match {
      case A => 14
      case K => 13
      case Q => 12
      case T => 10
      case J => -1
      case number => number.entryName.toInt
    }

    val ordering: Ordering[CardType] = Ordering.by(toInt)

    implicit val jokerOrdering: Ordering[CardType] = Ordering.by(toIntWithJokers)
  }

  sealed abstract class HandCategory(val value: Int, val name: String) extends IntEnumEntry
  object HandCategory extends IntEnum[HandCategory] {

    val values = findValues

    case object FiveOfAKind extends HandCategory(7, "fiveofakind")
    case object FourOfAKind extends HandCategory(6, "fourofakind")
    case object FullHouse extends HandCategory(5, "fullhouse")
    case object ThreeOfAKind extends HandCategory(4, "threeofakind")
    case object TwoPair extends HandCategory(3, "twopair")
    case object OnePair extends HandCategory(2, "onepair")
    case object HighCard extends HandCategory(1, "highcard")

    implicit val ordering: Ordering[HandCategory] = Ordering.by(_.value)
  }

  final case class Hand(cards: String, bid: Int) {
    lazy val category: HandCategory = {
      val cardList = cards.toCharArray.toList
      val distinctCards = cardList.distinct

      distinctCards.size match {
        case 1 => HandCategory.FiveOfAKind
        case 2 =>
          val duplicatesOfFirstDistinct = cardList.count(_ == distinctCards.head)
          if (duplicatesOfFirstDistinct == 1 || duplicatesOfFirstDistinct == 4)
            HandCategory.FourOfAKind
          else
            HandCategory.FullHouse
        case 3 =>
          // Either three of a kind or two pair
          // AAA12
          // AA112
          val hasThreeOfAKind = distinctCards.exists(card => cardList.count(_ == card) == 3)
          if (hasThreeOfAKind) HandCategory.ThreeOfAKind
          else HandCategory.TwoPair
        case 4 =>
          HandCategory.OnePair
        case 5 =>
          HandCategory.HighCard
      }
    }

    val jokerCategory: HandCategory = {
      val cardList = cards.toCharArray.toList
      val numJokers = cardList.count(_ == 'J')
      val nonJokerDistinct = cardList.filterNot(_ == 'J').distinct
      val numNonJokerDistinct = nonJokerDistinct.size

      numJokers match {
        case 5 =>
          HandCategory.FiveOfAKind
        case 4 =>
          HandCategory.FiveOfAKind
        case 3 =>
          if (numNonJokerDistinct == 1)
            HandCategory.FiveOfAKind
          else
            HandCategory.FourOfAKind
        case 2 =>
          if (numNonJokerDistinct == 1)
            HandCategory.FiveOfAKind
          else if (numNonJokerDistinct == 2)
            // 3 cards, two distinct card types. Must be two of one type, and one of another type.
            // Add on two Jokers and that makes four of a kind
            HandCategory.FourOfAKind
          else {
            // 3 cards, all distinct. Must be three of a kind with the two jokers.
            HandCategory.ThreeOfAKind
          }
        case 1 =>
          if (numNonJokerDistinct == 1)
            HandCategory.FiveOfAKind
          else if (numNonJokerDistinct == 2) {
            // 4 cards, two types. Possible splits are:
            //  - 3 of one type, 1 of the other type
            //  - 2 of each type
            val firstDistinctCount = nonJokerDistinct.count(_ == nonJokerDistinct.head)
            if (firstDistinctCount == 1 || firstDistinctCount == 3)
              HandCategory.FourOfAKind
            else
              HandCategory.FullHouse
          }
          else if (numNonJokerDistinct == 3) {
            // 4 cards, 3 types. Must be 2 of one card type, 1 each of the other card types
            HandCategory.ThreeOfAKind
          }
          else {
            // 4 cards, all distinct. Best we can do is one pair
            HandCategory.OnePair
          }
        case 0 =>
          category
      }
    }
  }

  object Hand {

    val ordering: Ordering[Hand] = new Ordering[Hand] {
      override def compare(x: Hand, y: Hand): Int = {
        if (x.category != y.category)
          Ordering[HandCategory].compare(x.category, y.category)
        else {
          x.cards.zip(y.cards).foldLeft[Option[Int]](None) {
            case (maybeComparison, (xCard, yCard)) =>
              // if we've already found a point of comparison, pass it along
              if (maybeComparison.nonEmpty)
                maybeComparison
              // Otherwise compare the cards here
              else {
                val xCardType = CardType.withName(xCard.toString)
                val yCardType = CardType.withName(yCard.toString)

                // If these cards are the same, move on to the next cards
                if (xCardType == yCardType)
                  None
                // If the cards are different, we can compare
                else
                  Some(Ordering[CardType].compare(xCardType, yCardType))
              }
          }.getOrElse(0) // if the comparison value was never updated, hands are completely equal
        }
      }
    }

    implicit val jokerOrdering: Ordering[Hand] = new Ordering[Hand] {
      override def compare(x: Hand, y: Hand): Int = {
        if (x.jokerCategory != y.jokerCategory)
          Ordering[HandCategory].compare(x.jokerCategory, y.jokerCategory)
        else {
          x.cards.zip(y.cards).foldLeft[Option[Int]](None) {
            case (maybeComparison, (xCard, yCard)) =>
              // if we've already found a point of comparison, pass it along
              if (maybeComparison.nonEmpty)
                maybeComparison
              // Otherwise compare the cards here
              else {
                val xCardType = CardType.withName(xCard.toString)
                val yCardType = CardType.withName(yCard.toString)

                // If these cards are the same, move on to the next cards
                if (xCardType == yCardType)
                  None
                // If the cards are different, we can compare
                else
                  Some(Ordering[CardType].compare(xCardType, yCardType))
              }
          }.getOrElse(0) // if the comparison value was never updated, hands are completely equal
        }
      }
    }
  }


  def part1(): IO[Int] = {

    // Step 1: parse the hands and bids
    def parse(lines: List[String]): List[Hand] = lines map { line =>
      line.split(" ").toList match {
        case List(cards, bid) => Hand(cards, bid.toInt)
      }
    }
    // Step 2: Sort the hands

    // Step 3: Determine winnings for each hand based on rank and bid
    def findWinnings(hands: List[Hand]): List[Int] = {
      hands
        .sorted
        .reverse
        .zipWithIndex
        .map { case (hand, index) =>
          val rank = hands.length - index

          println(s"Hand: $hand, rank: $rank")

          hand.bid * rank

        }
    }

    // Step 4: Sum all winnings to find total

    for {
      _ <- IO.println("Day7 part 1: calculating...")
      lines <- Input.loadAll[IO]("Day7.txt")
      hands = parse(lines)
      winnings = findWinnings(hands)
    } yield winnings.sumAll
  }

  def part2(): IO[Int] = {

    // Step 1: parse the hands and bids
    def parse(lines: List[String]): List[Hand] = lines map { line =>
      line.split(" ").toList match {
        case List(cards, bid) => Hand(cards, bid.toInt)
      }
    }
    // Step 2: Sort the hands

    // Step 3: Determine winnings for each hand based on rank and bid
    def findWinnings(hands: List[Hand]): List[Int] = {
      hands
        .sorted
        .reverse
        .zipWithIndex
        .map { case (hand, index) =>
          val rank = hands.length - index

          println(s"Hand: $hand, rank: $rank")

          hand.bid * rank

        }
    }

    // Step 4: Sum all winnings to find total

    for {
      _ <- IO.println("Day7 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day7.txt")
      hands = parse(lines)
      winnings = findWinnings(hands)
    } yield winnings.sumAll
  }

}
