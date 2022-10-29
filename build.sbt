ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.10"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "scalamidi" % "0.2.1",
      "org.typelevel" %% "cats-core" % "2.8.0"),
    name := "genome-player"
  )
