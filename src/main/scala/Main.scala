import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {

  val run: IO[Unit] = for {
    _ <- IO.println("Starting Advent of Code Solver...")
    result <- Day10.part1
    _ <- IO.println(s"Result: $result")
  } yield ()
}
