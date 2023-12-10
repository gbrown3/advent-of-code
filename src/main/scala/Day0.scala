import cats.implicits._
import cats.effect._

/**
 * Template for Advent of Code solutions
 */
object Day0 {

  def part1: IO[Int] = {

    for {
      _ <- IO.println("Day0 part 1: calculating...")
      lines <- Input.loadAll[IO]("Day0.txt")
    } yield 0
  }

  def part2: IO[Int] = {

    for {
      _ <- IO.println("Day0 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day0.txt")
    } yield 0
  }
}
