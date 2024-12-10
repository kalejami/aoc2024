package de.mk.aoc2024

import cats.effect.{IO, Resource}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Util {
  def getInput(name: String): Resource[IO, BufferedSource] =
    Resource.make(IO(scala.io.Source.fromResource(name)))(r => IO(r.close()))

  sealed trait Direction extends Product with Serializable
  object Direction {
    case object Up extends Direction
    case object Down extends Direction
    case object Left extends Direction
    case object Right extends Direction

    def turnRight(d: Direction): Direction = d match {
      case Up    => Right
      case Down  => Left
      case Left  => Up
      case Right => Down
    }
  }

  final case class Pos(x: Int, y: Int) {
    def +(p2: Pos): Pos = Pos(x + p2.x, y + p2.y)
    def -(p2: Pos): Pos = Pos(x - p2.x, y - p2.y)

    def nextPos(direction: Direction): Pos = {
      import Direction._
      direction match {
        case Up    => copy(y = y - 1)
        case Down  => copy(y = y + 1)
        case Left  => copy(x = x - 1)
        case Right => copy(x = x + 1)
      }
    }
  }

  final case class Grid(grid: Vector[Vector[Char]]) {
    def lookAhead(pos: Pos, direction: Direction): Option[Char] = {
      charAt(pos.nextPos(direction))
    }

    def charAt(p: Pos): Option[Char] = grid.lift(p.y).flatMap(_.lift(p.x))

    def inBounds(p: Pos): Boolean = {
      if (p.y < grid.size && p.y >= 0) {
        grid.headOption.exists(xs => p.x < xs.size && p.x >= 0)
      } else false
    }

    @tailrec
    private def indexOfLoop(
        c: Char,
        ys: Iterator[(Vector[Char], Int)],
        result: List[Pos]
    ): List[Pos] = ys.nextOption() match {
      case Some((xs, y)) =>
        val indices = xs.zipWithIndex.filter(_._1 == c).map(t => Pos(t._2, y))
        indexOfLoop(c, ys, result ++ indices)
      case None => result
    }

    def allIndexOf(c: Char): List[Pos] =
      indexOfLoop(c, grid.zipWithIndex.iterator, Nil)

    def indexOf(c: Char): Option[Pos] = allIndexOf(c).headOption
  }
}
