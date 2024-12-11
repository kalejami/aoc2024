package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.annotation.tailrec

object day11 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input11.txt")
    .use { bs =>
      val line = bs.getLines().toList.mkString

      val nums = line.split(" ").toVector.map(_.toLong)

      def blink(l: Long): Vector[Long] = {
        val ls = l.toString
        if (ls == "0") Vector(1L)
        else if (ls.length % 2 == 0) {
          val (x1, x2) = ls.splitAt(ls.length / 2)
          Vector(x1.toLong, x2.toLong)
        } else Vector((l * 2024))
      }

      @tailrec
      def loop(index: Int, limit: Int, acc: Vector[Long], accN: Long): Long = {
        if (index > limit) accN
        else {
          val newAcc = acc.flatMap { blink }
          val size = newAcc.size
          loop(index + 1, limit, newAcc, size.toLong)
        }
      }

      def optSolution(stones: Vector[Long], blinks: Int): Long = {
        var nextStones: Vector[Long] = stones
        var count = stones.groupBy(identity).view.mapValues(_.size.toLong).toMap

        for (_ <- 0 until blinks) {
          var nextCount: Map[Long, Long] = Map()
          for ((stone, stoneCount) <- count) {
            nextStones = blink(stone)
            for (nextStone <- nextStones) {
              nextCount = nextCount.updated(
                nextStone,
                nextCount.getOrElse(nextStone, 0L) + stoneCount
              )
            }
          }
          count = nextCount
        }
        count.values.sum
      }

      def optImmutable(xs: Vector[Long], blinks: Int): Long = {
        @tailrec
        def go(ns: List[(Long, Long)], counts: Map[Long, Long], blinks: Int)
            : Map[Long, Long] =
          ns match {
            case ::((nextStone, lastCount), next) =>
              val nextCounts = counts.updated(
                nextStone,
                counts.getOrElse(nextStone, 0L) + lastCount
              )
              go(next, nextCounts, blinks)
            case Nil =>
              if (blinks == 0) counts
              else {
                val nexts = counts.toList.flatMap { case (s, c) =>
                  blink(s).map(sn => (sn, c))
                }
                go(nexts, Map.empty, blinks - 1)
              }
          }
        go(
          List.empty,
          xs.groupBy(identity).map { case (s, xs) => (s, xs.size.toLong) },
          blinks
        ).values.sum
      }

      def a1: IO[Long] = IO(optImmutable(nums, 25))

      def a2: IO[Long] = IO(optImmutable(nums, 75))

      a1.flatMap(IO.println) >> a2.flatMap(IO.println)
    }
    .as(ExitCode.Success)
}
