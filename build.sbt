val scala3Version = "3.3.1"

val deps: List[ModuleID] = List (
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
  "org.typelevel" %% "cats-core" % "2.10.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "AOC2023",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= deps
  )
