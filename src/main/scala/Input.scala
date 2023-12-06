import cats.effect._
import fs2.io.file._
import fs2.Stream

object Input {

  private val PathToResources: String = "src/main/resources"

  def streamLines[F[_] : Files : Concurrent](fileName: String): Stream[F, String] = {
    val path: Path = Path(s"$PathToResources/$fileName")
    Files[F].readUtf8Lines(path)
  }

  /**
   * Loads entire file in resources folder.
   *
   * @param fileName - must include file extension, ie .txt
   * @tparam F - Effect type
   * @return An effect that returns a list of lines from the input file
   */
  def loadAll[F[_] : Files : Concurrent](fileName: String): F[List[String]] = {
    streamLines(fileName)
      .compile
      .toList
  }
}
