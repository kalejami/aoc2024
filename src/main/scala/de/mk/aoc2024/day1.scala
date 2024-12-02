package de.mk.aoc2024

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object day1 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Util
      .getInput("input1.txt")
      .use { bs =>
        val lines = bs.getLines
        val nums = lines.map { line =>
          val split = line.split("   ")
          (split(0).toLong, split(1).toLong)
        }.toList
        a1(nums) >> a2(nums)
      }
      .as(ExitCode.Success)

  def a1(nums: List[(Long, Long)]) = {
    val x1s = nums.map(_._1).sorted
    val x2s = nums.map(_._2).sorted
    val sorted = x1s.zip(x2s).map { case (x1, x2) =>
      math.abs(x1 - x2)
    }
    IO.println(sorted.sum)
  }

  def a2(nums: List[(Long, Long)]) = {
    val x1s = nums.map(_._1).sorted
    val x2s = nums.map(_._2).sorted

    val sims = x1s.map { x1 =>
      val mul = x2s.count(_ == x1)
      x1 * mul
    }

    IO.println(sims.sum)
  }

}
