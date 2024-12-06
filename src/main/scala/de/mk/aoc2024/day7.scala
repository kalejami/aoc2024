package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.annotation.tailrec

object day7 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input7.txt")
    .use { bs =>
      val lines = bs.getLines().toList

      def a1: Long = 0L

      def a2: Long = 0L

      IO.println(a1) >> IO.println(a2)
    }
    .as(ExitCode.Success)
}
