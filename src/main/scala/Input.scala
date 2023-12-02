import cats.effect.{IO, Resource}

import scala.io.Source

object Input {

  /**
   * Loads entire file, concatenating into a newline-separated string
   */
  def loadAll(fileName: String): IO[List[String]] = {
    val file: Resource[IO, Source] = Resource.make(
      IO(Source.fromResource(fileName))
    )(source => IO(source.close()))

//    file.use(source => IO(source.getLines().mkString("\n")))
    file.use(source => IO(source.getLines().toArray.toList))
  }
}
