package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.aoc2024.Util.Direction.{Down, Up}
import de.mk.aoc2024.Util.{Direction, Grid, Pos}

import scala.annotation.tailrec

object day10 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input10.txt")
    .use { bs =>
      val lines = bs.getLines().toList

      val grid = Grid(lines.toVector.map(_.toVector))

      val startingPoses = grid.allIndexOf('0')

      def findNexts(p: Pos, index: Int): List[Pos] =
        List(Direction.Up, Direction.Down, Direction.Left, Direction.Right)
          .map(p.nextPos)
          .mapFilter { p =>
            val optChar = grid.charAt(p)
            val filtered = optChar.filter(_.toString == index.toString)
            filtered.map(_ => p)
          }

      @tailrec
      def findPath(index: Int, next: List[Pos]): List[Pos] = {
        if (index > 9) next
        else {
          val newNextes = next.flatMap { n => findNexts(n, index) }
          if (newNextes.isEmpty) List.empty else findPath(index + 1, newNextes)
        }
      }

      val paths = startingPoses.map { p =>
        val nexts = findNexts(p, 1)
        (p, findPath(2, nexts))
      }

      case class Path(start: Pos, next: List[Pos])

      println(startingPoses)

      println(paths)

      def a1: Long = paths.map(_._2.distinct.size).sum

      def a2: Long = paths.map(_._2.size).sum

      IO.println(a1) >> IO.println(a2)
    }
    .as(ExitCode.Success)
}
