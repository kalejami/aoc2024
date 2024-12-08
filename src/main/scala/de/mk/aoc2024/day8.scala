package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.aoc2024.Util.Pos

import scala.annotation.tailrec

object day8 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input8.txt")
    .use { bs =>
      val lines = bs.getLines().toList

      val grid = Util.Grid(lines.toVector.map(_.toVector))

      val antennas = grid.grid.zipWithIndex
        .flatMap { case (chars, y) =>
          chars.zipWithIndex.flatMap { case (c, x) =>
            if (c != '.') Some(c, Pos(x, y)) else None
          }
        }
        .groupMap(_._1)(_._2)

      def countAntinodes(getAntinodes: (Pos, Pos) => Set[Pos]) = antennas
        .flatMap { case (_, poses) =>
          poses.flatMap(it1 =>
            poses.flatMap(it2 =>
              if (it1 != it2) getAntinodes(it1, it2) else Set.empty
            )
          )
        }
        .toSet
        .size

      def a1: Long = countAntinodes { (p1, p2) =>
        val diff = p2 - p1
        Set(p1 - diff, p2 + diff).filter(grid.inBounds)
      }

      def a2: Long = countAntinodes { (p1, p2) =>
        @tailrec
        def loop(currentPos: Pos, acc: Set[Pos], fn: Pos => Pos): Set[Pos] =
          if (grid.inBounds(currentPos)) {
            val newSet = acc + currentPos
            loop(fn(currentPos), newSet, fn)
          }
          else acc

        val diff = p2 - p1
        val a1 = loop(p1, Set.empty, _ - diff)
        val a2 = loop(p2, Set.empty, _ + diff)
        a1 ++ a2
      }

      IO.println(a1) >> IO.println(a2)
    }
    .as(ExitCode.Success)
}
