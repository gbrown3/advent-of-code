import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {

  val run: IO[Unit] = for {
    _ <- IO.println("Starting Advent of Code Solver...")
    result <- Day1.sumCoordinatesAdvanced()
    _ <- IO.println(s"Result: $result")
  } yield ()
}
