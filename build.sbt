// Resolves an issue with running Cats Effect in interactive sbt session
Compile / run / fork := true

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := "2.13.12",

    libraryDependencies ++= List(
      "org.typelevel" %% "cats-effect" % "3.5.2",
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
