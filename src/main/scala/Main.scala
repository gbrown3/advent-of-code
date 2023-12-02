//@main def hello: Unit =
//  println("Hello world!")
//  println(msg)
//
//def msg = "I was compiled by Scala 3. :)"

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  val run = IO.println("Hello, World!")
}
