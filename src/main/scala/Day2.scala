import cats.implicits._
import cats.effect._

object Day2 {

  /*
  As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue.
  Each time you play this game, he will hide a secret number of cubes of each color in the bag,
  and your goal is to figure out information about the number of cubes.

  Input:
  Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
  Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
  Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
  Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
  Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

  Determine which games would have been possible if the bag had been loaded with
  only 12 red cubes, 13 green cubes, and 14 blue cubes.

  What is the sum of the IDs of those games?
   */
  final case class CubeSet(red: Int, green: Int, blue: Int) {
    lazy val power = red * green * blue
  }
  object CubeSet {
    val empty: CubeSet = CubeSet(0, 0, 0)
  }

  final case class Game(id: Int, cubeSets: List[CubeSet])

  val MaxRed: Int = 12
  val MaxGreen: Int = 13
  val MaxBlue: Int = 14

  val Red: String = "red"
  val Green: String = "green"
  val Blue: String = "blue"

  def extractColorCount(string: String): Int = string.split(" ")(0).toInt

  // TODO: actually put in error handling
  def parseGame(string: String): Game = {
    string.split(":").toList match {
      case idString :: cubeSetsString =>
        val id: Int = idString.split(" ")(1).toInt

        val cubeSets: List[CubeSet] = cubeSetsString.head.split(";").toList map { cubeSetString =>
          cubeSetString.split(",").toList.foldLeft(CubeSet(0, 0, 0)) { (cubeSet, colorString) =>
            colorString.trim match {
              case string if string.contains(Red) =>
                cubeSet.copy(red = extractColorCount(string))
              case string if string.contains(Green) =>
                cubeSet.copy(green = extractColorCount(string))
              case string if string.contains(Blue) =>
                cubeSet.copy(blue = extractColorCount(string))
            }
          }
        }
        Game(id, cubeSets)
    }
  }

  def sumPossibleGames(): IO[Int] = {

    def isPossible(game: Game): Boolean =
      !game.cubeSets.exists( cubeSet =>
        cubeSet.red > MaxRed || cubeSet.green > MaxGreen || cubeSet.blue > MaxBlue
      )

    for {
      lines <- Input.loadAll[IO]("Day2.txt")
      games = lines.map(parseGame)
      possibleGames = games.filter(isPossible)
      ids = possibleGames.map(_.id)
      _ <- IO.println(s"parsed games: $games")
      _ <- IO.println(s"possible games: $possibleGames")
    } yield ids.sumAll

  }

  /*
  Approach - how do determine minimum num of each cube?
  Scan through every cubeSet, keeping track of the max for each color.
   */
  def sumMinimumCubePowers(): IO[Int] = {

    def findMinimumCubeSet(game: Game): CubeSet = {
      game.cubeSets.foldLeft(CubeSet.empty) { (minimumCubeSet, gameCubeSet) =>
        val updatedRed =
          if (gameCubeSet.red > minimumCubeSet.red) minimumCubeSet.copy(red = gameCubeSet.red)
          else minimumCubeSet

        val updatedGreenAndRed =
          if (gameCubeSet.green > updatedRed.green) updatedRed.copy(green = gameCubeSet.green)
          else updatedRed

        if (gameCubeSet.blue > updatedGreenAndRed.blue) updatedGreenAndRed.copy(blue = gameCubeSet.blue)
        else updatedGreenAndRed
      }
    }

    for {
      lines <- Input.loadAll[IO]("Day2.txt")
      games = lines.map(parseGame)
      minimumCubeSets = games.map(findMinimumCubeSet)
      powers = minimumCubeSets.map(_.power)
    } yield powers.sumAll
  }
}
