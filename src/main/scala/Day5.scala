import cats.implicits._
import cats.effect._
import enumeratum._
import enumeratum.EnumEntry._

import scala.collection.immutable.NumericRange

object Day5 {

  final case class InputMap(destinationStart: Long, sourceStart: Long, rangeLength: Long) {

    lazy val sourceRange: NumericRange.Exclusive[Long] = Range.Long(sourceStart, sourceStart + rangeLength, 1)
  }

  final case class FarmMap(inputMaps: List[InputMap] = List.empty) {

    def lookup(source: Long): Long = {
      inputMaps.foldLeft(source) { case (result, inputMap) =>
        if (inputMap.sourceRange.contains(source)) {
          val sourceDiff = source - inputMap.sourceStart
          inputMap.destinationStart + sourceDiff
        }
        else
          result
      }
    }
  }

  final case class AllInput(
    seeds: List[Long],
    seedToSoil: FarmMap,
    soilToFertilizer: FarmMap,
    fertilizerToWater: FarmMap,
    waterToLight: FarmMap,
    lightToTemp: FarmMap,
    tempToHumidity: FarmMap,
    humidityToLocation: FarmMap
   ) {
    lazy val seedLocations: List[Long] = {
      seeds.map { seed =>
        humidityToLocation.lookup(
          tempToHumidity.lookup(
            lightToTemp.lookup(
              waterToLight.lookup(
                fertilizerToWater.lookup(
                  soilToFertilizer.lookup(
                    seedToSoil.lookup(
                      seed
                    )
                  )
                )
              )
            )
          )
        )
      }
    }
  }


  sealed trait ParseState extends EnumEntry
  object ParseState extends Enum[ParseState] {

    val values: IndexedSeq[ParseState] = findValues
    case object SearchingForMap extends ParseState
    case object BuildingMap extends ParseState

    case object FoundAllMaps extends ParseState
  }

  sealed trait MapType extends EnumEntry with Hyphencase
  object MapType extends Enum[MapType] {

    val values: IndexedSeq[MapType] = findValues

    case object SeedToSoil extends MapType
    case object SoilToFertilizer extends MapType
    case object FertilizerToWater extends MapType
    case object WaterToLight extends MapType
    case object LightToTemperature extends MapType
    case object TemperatureToHumidity extends MapType
    case object HumidityToLocation extends MapType
  }

  final case class StreamState(
    parseState: ParseState = ParseState.SearchingForMap,
    currentMapId: Option[MapType] = None,
    seeds: List[Long] = List.empty,
    maps: Map[MapType, FarmMap] = Map.empty
  )

  /*
  Parsing logic

  Does this line have a colon?
    Yes -> must be the start of a new input value. Figure out which one.
      Does this contain the string "seeds:" ?
        Yes -> all seeds on current line. Save and move one.
        No ->
          Must be a map. Figure out which one by checking a unique string, then follow the algorithm below:

          Set state to Building Map, save map ID, move to next line
          Is this line all whitespace?
            Yes -> Stop building map. Set state to Search


   */

  def parseInput(fileName: String): IO[AllInput] = {
    Input
      .streamLines[IO](fileName)
      .scan(StreamState()) { (streamState, nextLine) =>
        nextLine match {
          case nextLine if nextLine.contains("seeds:") =>
            val seeds = nextLine
              .split(':')
              .toList
              .last
              .trim
              .split(" ")
              .toList
              .map(_.toLong)
            streamState.copy(seeds = seeds)
          case nextLine if nextLine.contains(":") =>
            val mapTypeString = nextLine
              .split(':')
              .toList
              .head
              .split(" ")
              .toList
              .head

            val mapType = MapType.withName(mapTypeString)
            streamState.copy(
              currentMapId = Some(mapType),
              parseState = ParseState.BuildingMap,
              maps = streamState.maps.updated(mapType, FarmMap())
            )
          case nextLine if nextLine.exists(_.isDigit) =>
            // We have more input for the current map type
            val newInputMap = nextLine.split(" ").toList match {
              case destinationStart :: sourceStart :: rangeLength :: Nil =>
                InputMap(destinationStart.toLong, sourceStart.toLong, rangeLength.toLong)
            }
            val currentMapId = streamState.currentMapId.get
            val farmMap = streamState.maps(currentMapId)

            streamState.copy(
              maps = streamState.maps.updated(
                currentMapId,
                farmMap.copy(
                  inputMaps = farmMap.inputMaps :+ newInputMap
                )
              )
            )
          case nextLine if nextLine.isBlank =>
            streamState.copy(
              parseState = ParseState.SearchingForMap,
              currentMapId = None
            )
        }
      }
      .compile
      .lastOrError
      .map { finalStreamState =>
        AllInput(
          seeds = finalStreamState.seeds,
          seedToSoil = finalStreamState.maps(MapType.SeedToSoil),
          soilToFertilizer = finalStreamState.maps(MapType.SoilToFertilizer),
          fertilizerToWater = finalStreamState.maps(MapType.FertilizerToWater),
          waterToLight = finalStreamState.maps(MapType.WaterToLight),
          lightToTemp = finalStreamState.maps(MapType.LightToTemperature),
          tempToHumidity = finalStreamState.maps(MapType.TemperatureToHumidity),
          humidityToLocation = finalStreamState.maps(MapType.HumidityToLocation)
        )
      }
  }

  def part1(): IO[Long] = {

    for {
      _ <- IO.println("Day5 part 1: calculating...")
      allInput <- parseInput("Day5.txt")
    } yield allInput.seedLocations.min
  }

  def part2(): IO[Int] = {

    for {
      _ <- IO.println("Day5 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day5.txt")
    } yield 0
  }

}
