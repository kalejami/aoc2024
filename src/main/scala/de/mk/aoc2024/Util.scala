package de.mk.aoc2024

import cats.effect.{IO, Resource}

import scala.io.BufferedSource

object Util {
  def getInput(name: String): Resource[IO, BufferedSource] =
    Resource.make(IO(scala.io.Source.fromResource(name)))(r => IO(r.close()))
}
