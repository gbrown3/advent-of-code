import cats.implicits._
import cats.effect._

object Day6 {

  final case class Race(timeLimit: Int, recordDistance: Int)

  final case class BigRace(timeLimit: Long, recordDistance: Long)
  def part1(): IO[Int] = {

    def parse(lines: List[String]): List[Race] = {
      lines match {
        case List(timeString, distanceString) =>
          val times = timeString.split(':').toList.last.trim.split(" ").toList.filterNot(_.isBlank).map(_.toInt)
          val distances = distanceString.split(':').toList.last.trim.split(" ").toList.filterNot(_.isBlank).map(_.toInt)
          times.zip(distances).map { case (time, distance) => Race(time, distance) }
      }
    }

    def countWaysToBeatRecord(race: Race): Int = {
      Range(0, race.timeLimit).toList.count { holdTime =>
        val distanceTraveled = (race.timeLimit - holdTime) * holdTime
        distanceTraveled > race.recordDistance
      }
    }

    for {
      _ <- IO.println("Day6 part 1: calculating...")
      lines <- Input.loadAll[IO]("Day6.txt")
      races = parse(lines)
      solutionsPerRace = races.map(countWaysToBeatRecord)
    } yield solutionsPerRace.productAll
  }

  def part2(): IO[Long] = {

    def parse(lines: List[String]): BigRace = {
      lines match {
        case List(timeString, distanceString) =>
          val time = timeString.split(':').toList.last.filterNot(_.isWhitespace).toLong
          val distance = distanceString.split(':').toList.last.filterNot(_.isWhitespace).toLong
          BigRace(time, distance)
      }
    }

    def countWaysToBeatRecord(race: BigRace): Long = {
      Range.Long(0, race.timeLimit, 1).toList.count { holdTime =>
        val distanceTraveled = (race.timeLimit - holdTime) * holdTime
        distanceTraveled > race.recordDistance
      }
    }

    for {
      _ <- IO.println("Day6 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day6.txt")
      race = parse(lines)
    } yield countWaysToBeatRecord(race)
  }

}
