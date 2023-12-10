import cats.implicits._
import cats.effect._

import enumeratum._

import scala.annotation.tailrec

object Day10 {

  final case class Point(x: Int, y: Int)

  sealed trait Direction extends EnumEntry
  object Direction extends Enum[Direction] {
    val values = findValues

    case object North extends Direction
    case object East extends Direction
    case object South extends Direction
    case object West extends Direction
  }

  final case class PipeVector(position: Point, direction: Direction)

  def part1: IO[Int] = {

    // Approach: farthest away from animal is 1/2 of total pipe length. Count up all pipes connected to starting loop, then reduce by half.


    def totalPipes(grid: List[List[Char]]): Int = {

      // Find starting position
      val indexedGrid = grid.map(_.zipWithIndex).zipWithIndex

      val startPos = indexedGrid.collect {
        case (row, y) if row.exists(_._1 == 'S') =>
          val x = row.find(_._1 == 'S').get._2
          (x, y)
      }

      // Track the pipe

      /*
      | is a vertical pipe connecting north and south.
      - is a horizontal pipe connecting east and west.
      L is a 90-degree bend connecting north and east.
      J is a 90-degree bend connecting north and west.
      7 is a 90-degree bend connecting south and west.
      F is a 90-degree bend connecting south and east.
       */
      def getNextPointAndDirection(pipeVector: PipeVector): PipeVector = {
        val currentPoint = pipeVector.position
        val direction = pipeVector.direction

        val currentPointChar = grid(currentPoint.y)(currentPoint.x)

        (currentPointChar, direction) match {
          case ('|', Direction.North) => PipeVector(
            Point(currentPoint.x, currentPoint.y + 1),
            Direction.North
          )
          case ('|', Direction.South) => PipeVector(
            Point(currentPoint.x, currentPoint.y - 1),
            Direction.South
          )
          case ('-', Direction.East) => PipeVector(
            Point(currentPoint.x + 1, currentPoint.y),
            Direction.East
          )
          case ('-', Direction.West) => PipeVector(
            Point(currentPoint.x - 1, currentPoint.y),
            Direction.West
          )
          case ('L', Direction.West) => PipeVector(
            Point(currentPoint.x, currentPoint.y + 1),
            Direction.North
          )
          case ('L', Direction.South) => PipeVector(
            Point(currentPoint.x + 1, currentPoint.y),
            Direction.East
          )
          case ('J', Direction.East) => PipeVector(
            Point(currentPoint.x, currentPoint.y + 1),
            Direction.North
          )
          case ('J', Direction.South) => PipeVector(
            Point(currentPoint.x - 1, currentPoint.y),
            Direction.West
          )
          case ('7', Direction.East) => PipeVector(
            Point(currentPoint.x, currentPoint.y - 1),
            Direction.South
          )
          case ('7', Direction.North) => PipeVector(
            Point(currentPoint.x - 1, currentPoint.y),
            Direction.West
          )
          case ('F', Direction.North) => PipeVector(
            Point(currentPoint.x + 1, currentPoint.y),
            Direction.East
          )
          case ('F', Direction.West) => PipeVector(
            Point(currentPoint.x, currentPoint.y - 1),
            Direction.South
          )
        }
      }

      @tailrec
      def followPipe(pipeVector: PipeVector, steps: Int): Int =
        if (grid(pipeVector.position.y)(pipeVector.position.x) == 'S') steps
        else
          followPipe(
            getNextPointAndDirection(pipeVector),
            steps + 1
          )

      // TODO: find first pipe connecting from start position, then follow pipe and count length, then cut in half
    }

    for {
      _ <- IO.println("Day10 part 1: calculating...")
      lines <- Input.loadAll[IO]("Day10-sample.txt")
    } yield 0
  }

  def part2: IO[Int] = {

    for {
      _ <- IO.println("Day10 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day10-sample.txt")
    } yield 0
  }
}
