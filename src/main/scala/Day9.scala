import cats.implicits._
import cats.effect._

import scala.annotation.tailrec

object Day9 {

  def getNextNumSequence(numSequence: List[Int]): List[Int] = {
    numSequence.sliding(2).fold(List.empty[Int]) { case (nextSequence, List(prev, next)) =>
      nextSequence :+ (next - prev)
    }
  }

  @tailrec
  def getAllSequences(numSequence: List[Int], allSequences: List[List[Int]]): List[List[Int]] =
    if (numSequence.forall(_ == 0)) allSequences :+ numSequence
    else getAllSequences(
      getNextNumSequence(numSequence),
      allSequences :+ numSequence
    )

  def part1: IO[Int] = {

    def extrapolate(line: String): Int = {
      val ogNumSequence = line.split(" ").toList.map(_.toInt)

      val allSequences = getAllSequences(ogNumSequence, List.empty)

      allSequences.reverse.foldLeft(0) { case (extrapValue, nextSequence) =>
        extrapValue + nextSequence.last
      }
    }

    for {
      _ <- IO.println("Day9 part 1: calculating...")
      lines <- Input.loadAll[IO]("Day9.txt")
      extrapolated = lines.map(extrapolate)
    } yield extrapolated.sumAll
  }

  def part2: IO[Int] = {

    def extrapolate(line: String): Int = {
      val ogNumSequence = line.split(" ").toList.map(_.toInt)
      val allSequences = getAllSequences(ogNumSequence, List.empty)

//      println(s"all sequences: $allSequences")

      allSequences.reverse.foldLeft(0) { case (extrapValue, nextSequence) =>
        val extrapolatedVal = nextSequence.head - extrapValue

//        println(s"extrapolated: $extrapolatedVal for sequence $nextSequence")

        extrapolatedVal

      }
    }

    for {
      _ <- IO.println("Day9 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day9.txt")
      extrapolated = lines.map(extrapolate)
    } yield extrapolated.sumAll
  }
}
