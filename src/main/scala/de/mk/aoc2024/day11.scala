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

      @tailrec
      def loop(index: Int, limit: Int, acc: Vector[String]): Vector[String] = {
        if (index > limit) acc
        else {
          val newAcc = acc.flatMap { n =>
            if (n == "0") Vector("1")
            else if (n.length % 2 == 0) {
              val (x1, x2) = n.splitAt(n.length / 2)
              Vector(x1, x2.toLong.toString)
            } else Vector((n.toLong * 2024).toString)
          }
          loop(index + 1, limit, newAcc)
        }
      }


      def a1: IO[Long] = {
        val parts = nums.parTraverse( n =>
          IO(loop(0, 24, Vector(n))).map(_.size.toLong)
        )
        parts.map(_.sum)
      }

      def a2: IO[Long] = {
        val parts = nums.parTraverse( n =>
          IO(loop(0, 74, Vector(n))).map(_.size.toLong)
        )
        parts.map(_.sum)
      }

      a1.flatMap(IO.println) >> a2.flatMap(IO.println)
    }
    .as(ExitCode.Success)
}
