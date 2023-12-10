import cats.implicits._
import cats.effect._

import scala.annotation.tailrec

object Day8 {

  final case class CamelMap(instructions: List[Char], nodes: Map[String, (String, String)])

  def parse(lines: List[String]): CamelMap = {
    lines.filterNot(_.isBlank) match {
      case instructions :: nodeStrings =>
        val nodes = nodeStrings.map { nodeString =>
          nodeString.split(" = ").toList match {
            case List(key, valueString) =>
              val values = valueString
                .split(", ")
                .toList
                .map(_.filter(_.isLetter)) match {
                case List(leftNode, rightNode) => (leftNode, rightNode)
              }

              key -> values
          }
        }.toMap

        CamelMap(instructions.toCharArray.toList, nodes)
    }
  }

  def part1(): IO[Int] = {

    /*
    How many steps to get to ZZZ?

    Step 1: Parse input into instructions and node map

    Step 2: Recursively follow instructions until ZZZ is found, counting number of recursions
     */

    def findZZZ(camelMap: CamelMap): Int = {

      @tailrec
      def recurse(node: String, instructionIndex: Int, count: Int): Int = {
        if (node == "ZZZ") count
        else recurse(
          camelMap.nodes(node) match {
            case (left, _) if camelMap.instructions(instructionIndex) == 'L' => left
            case (_, right) => right
          },
          (instructionIndex + 1) % camelMap.instructions.length,
          count + 1
        )
      }

      recurse("AAA", 0, 0)
    }

    for {
      _ <- IO.println("Day8 part 1: calculating...")
      lines <- Input.loadAll[IO]("Day8.txt")
      camelMap = parse(lines)
      zzzIterations = findZZZ(camelMap)
    } yield zzzIterations
  }

  def part2(): IO[Int] = {

    def findZZZ(camelMap: CamelMap): Int = {
      @tailrec
      def recurse(nodes: List[String], instructionIndex: Int, count: Int): Int = {
        if (nodes.forall(_.endsWith("Z"))) count
        else recurse(
          nodes.map { node =>
            camelMap.nodes(node) match {
              case (left, _) if camelMap.instructions(instructionIndex) == 'L' => left
              case (_, right) => right
            }
          },
          (instructionIndex + 1) % camelMap.instructions.length,
          count + 1
        )
      }

      recurse(camelMap.nodes.keys.filter(_.endsWith("A")).toList, 0, 0)
    }

//    def findZZZPar(camelMap: CamelMap): IO[Int] = {
//
//      @tailrec
//      def recurse(nodes: List[String], instructionIndex: Int, count: Int): IO[Int] = {
//        if (nodes.forall(_.endsWith("Z"))) count.pure[IO]
//        else
//          recurse(
//          nodes.map { node =>
//            camelMap.nodes(node) match {
//              case (left, _) if camelMap.instructions(instructionIndex) == 'L' => left
//              case (_, right) => right
//            }
//          },
//          (instructionIndex + 1) % camelMap.instructions.length,
//          count + 1
//        )
//      }
//
//      recurse(camelMap.nodes.keys.filter(_.endsWith("A")).toList, 0, 0)
//    }

    for {
      _ <- IO.println("Day8 part 2: calculating...")
      lines <- Input.loadAll[IO]("Day8.txt")
      camelMap = parse(lines)
      zzzIterations = findZZZ(camelMap)
    } yield zzzIterations
  }
}
