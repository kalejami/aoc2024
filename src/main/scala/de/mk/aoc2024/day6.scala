package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._
import de.mk.aoc2024.Util.Direction.Up
import de.mk.aoc2024.Util.{Direction, Grid, Pos}

import scala.annotation.tailrec

object day6 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input6.txt")
    .use { bs =>
      val lines = bs.getLines().toList
      val grid = Grid(lines.toVector.map(_.toVector))

      val startPos = grid.indexOf('^')

      @tailrec
      def loop(
          g: Grid,
          currentPos: Pos,
          direction: Direction,
          visits: List[(Direction, Pos)]
      ): List[(Direction, Pos)] = {
        g.lookAhead(currentPos, direction) match {
          case Some(n) =>
            if (n == '#') {
              val newDir = Direction.turnRight(direction)
              loop(g, currentPos, newDir, visits)
            } else {
              val next = currentPos.nextPos(direction)
              loop(g, next, direction, (direction, next) :: visits)
            }
          case None => visits
        }
      }

      @tailrec
      def loopCircleDect(
          g: Grid,
          currentPos: Pos,
          direction: Direction,
          visits: List[(Direction, Pos)]
      ): Boolean = {
        if (visits != visits.distinct) true
        else {
          g.lookAhead(currentPos, direction) match {
            case Some(n) =>
              if (n == '#')
                loopCircleDect(
                  g,
                  currentPos,
                  Direction.turnRight(direction),
                  visits
                )
              else {
                val next = currentPos.nextPos(direction)
                loopCircleDect(g, next, direction, (direction, next) :: visits)
              }
            case None => false
          }
        }
      }

      val a1Loop = loop(grid, startPos.get, Direction.Up, Nil)

      def a1: Long = a1Loop.map(_._2).distinct.size

      val gridsWithNewO: Vector[Grid] = grid.grid.zipWithIndex.flatMap {
        case (xs, y) =>
          xs.zipWithIndex.flatMap { case (c, x) =>
            if (c == '#') None
            else Some(Grid(grid.grid.updated(y, xs.updated(x, '#'))))
          }
      }

      // Bruteforce FTW!
      @tailrec
      def a2Loop(
          l: List[Grid],
          loopCount: Int
      ): Int = l match {
        case ::(head, next) =>
          if (loopCircleDect(head, startPos.get, Up, Nil))
            a2Loop(next, loopCount + 1)
          else a2Loop(next, loopCount)
        case Nil => loopCount
      }

      def a2 = a2Loop(gridsWithNewO.toList, 0)

      IO.println(a1) >>
        IO.println(a2)
    }
    .as(ExitCode.Success)
}
