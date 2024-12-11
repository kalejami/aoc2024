package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.annotation.tailrec

object day11 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input11.txt")
    .use { bs =>
      val line = bs.getLines().toList.mkString

      val nums = line.split(" ").toVector

      def blink(n: String): Vector[String] =
        if (n == "0") Vector("1")
        else if (n.length % 2 == 0) {
          val (x1, x2) = n.splitAt(n.length / 2)
          Vector(x1, x2.toLong.toString)
        } else Vector((n.toLong * 2024).toString)

      def blinkS(n: String) = blink(n).mkString(" ")

      @tailrec
      def loop(index: Int, limit: Int, acc: Vector[String]): Vector[String] = {
        if (index > limit) acc
        else {
          val newAcc = acc.flatMap { blink }
          loop(index + 1, limit, newAcc)
        }
      }

      @tailrec
      def loop2(index: Int, limit: Int, acc: String): String = {
        if (index > limit) acc
        else {
          val newAcc = acc.split(" ").map(blinkS).mkString(" ")
          loop2(index + 1, limit, newAcc)
        }
      }

      def a1: IO[Long] = {
        val parts =
          nums.parTraverse(n => IO(loop2(0, 24, n)).map(_.count(_ == ' ').toLong + 1))
        parts.map(_.sum)
      }

      def a2: IO[Long] = {
        val parts =
          nums.parTraverse(n => IO(loop2(0, 74, n)).map(_.count(_ == ' ').toLong + 1))
        parts.map(_.sum)
      }

      a1.flatMap(IO.println) >> a2.flatMap(IO.println)
    }
    .as(ExitCode.Success)
}
