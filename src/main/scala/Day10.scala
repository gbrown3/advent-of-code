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

    // Approach: farthest away from animal is 1/2 of total pipe length, rounded up. Count up all pipes connected to starting loop, then reduce by half.


    def totalPipes(grid: List[List[Char]]): Int = {

      val reversedGrid = grid.reverse

      // Find starting position
      val indexedGrid = reversedGrid.map(_.zipWithIndex).zipWithIndex

      val startPos = indexedGrid.collect {
        case (row, y) if row.exists(_._1 == 'S') =>
          val x = row.find(_._1 == 'S').get._2
          (x, y)
      }.head

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

        val currentPointChar = reversedGrid(currentPoint.y)(currentPoint.x)

        // TODO: may need to reverse y changes to account for the fact that they're indices, not actual points
        // Alternatively, reverse the outer list of lines
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
        if (reversedGrid(pipeVector.position.y)(pipeVector.position.x) == 'S') steps
        else
          followPipe(
            getNextPointAndDirection(pipeVector),
            steps + 1
          )

      // TODO: find first pipe connecting from start position, then follow pipe and count length, then cut in half

      val firstPipeVector = {
        /*
        | is a vertical pipe connecting north and south.
        - is a horizontal pipe connecting east and west.
        L is a 90-degree bend connecting north and east.
        J is a 90-degree bend connecting north and west.
        7 is a 90-degree bend connecting south and west.
        F is a 90-degree bend connecting south and east.
         */
        val above = reversedGrid(startPos._2 + 1)(startPos._1)
        val right = reversedGrid(startPos._2)(startPos._1 + 1)
        val below = reversedGrid(startPos._2 - 1)(startPos._1)

        if (above == '|' || above == '7' || above == 'F')
          PipeVector(
            Point(x = startPos._1, y = startPos._2 + 1),
            Direction.North
          )
        else if (right == '-' || right == 'J' || right == '7')
          PipeVector(
            Point(x = startPos._1 + 1, y = startPos._2),
            Direction.East
          )
        else if (below == '|' || below == 'L' || below == 'J')
          PipeVector(
            Point(x = startPos._1, y = startPos._2 - 1),
            Direction.South
          )
        else
          PipeVector(
            Point(x = startPos._1 - 1, y = startPos._2),
            Direction.West
          )
      }

      followPipe(firstPipeVector, 1)
    }

    for {
      _ <- IO.println("Day10 part 1: calculating...")
      lines <- Input.loadAll[IO]("Day10.txt")
      totalPipeLength = totalPipes(lines.map(_.toCharArray.toList))
    } yield Math.ceil(totalPipeLength / 2).toInt
  }

  def part2: IO[Int] = {

    for {
      _ <- IO.println("Day10 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day10-sample.txt")
    } yield 0
  }
}
