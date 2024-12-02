ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "de.mk"
ThisBuild / organizationName := "mk"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2024",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.7"
    ),
    Compile / run / fork := true
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
