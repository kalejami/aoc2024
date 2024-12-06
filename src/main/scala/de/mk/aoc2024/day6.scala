package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.annotation.tailrec

object day6 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input6.txt")
    .use { bs =>
      val lines = bs.getLines().toList
      val linesWithIndex: List[(String, Int)] = lines.zipWithIndex

      sealed trait Direction
      case object Up extends Direction
      case object Down extends Direction
      case object Left extends Direction
      case object Right extends Direction

      val startPos =
        linesWithIndex
          .find(_._1.contains("^"))
          .flatMap(s =>
            s._1.zipWithIndex.find(_._1 == '^').map(c => (c._2, s._2))
          )

      def charAt(l: List[(String, Int)])(x: Int, y: Int): Option[Char] = l
        .find(_._2 == y)
        .flatMap(_._1.zipWithIndex.toList.find(_._2 == x).map(_._1))

      println(startPos)

      def nextPos(pos: (Int, Int), direction: Direction) = {
        direction match {
          case Up    => (pos._1, pos._2 - 1)
          case Down  => (pos._1, pos._2 + 1)
          case Left  => (pos._1 - 1, pos._2)
          case Right => (pos._1 + 1, pos._2)
        }
      }

      def turnRight(direction: Direction) = direction match {
        case Up    => Right
        case Down  => Left
        case Left  => Up
        case Right => Down
      }

      def lookAhead(
          l: List[(String, Int)]
      )(pos: (Int, Int), direction: Direction): Option[Char] = {
        val (x, y) = nextPos(pos, direction)
        charAt(l)(x, y)
      }

      @tailrec
      def loop(
          l: List[(String, Int)],
          currentPos: (Int, Int),
          direction: Direction,
          visits: List[(Direction, (Int, Int))]
      ): List[(Direction, (Int, Int))] = {
        lookAhead(l)(currentPos, direction) match {
          case Some(n) =>
            if (n == '#') loop(l, currentPos, turnRight(direction), visits)
            else {
              val next = nextPos(currentPos, direction)
              loop(l, next, direction, (direction, next) :: visits)
            }
          case None => visits
        }
      }

      @tailrec
      def loopCircleDect(
          l: List[(String, Int)],
          currentPos: (Int, Int),
          direction: Direction,
          visits: List[(Direction, (Int, Int))]
      ): Boolean = {
        if (visits != visits.distinct) true
        else {
          lookAhead(l)(currentPos, direction) match {
            case Some(n) =>
              if (n == '#')
                loopCircleDect(l, currentPos, turnRight(direction), visits)
              else {
                val next = nextPos(currentPos, direction)
                loopCircleDect(l, next, direction, (direction, next) :: visits)
              }
            case None => false
          }
        }
      }

      val a1Loop = loop(linesWithIndex, startPos.get, Up, Nil)

      def a1: Long = a1Loop.map(_._2).distinct.size

      val maxY = linesWithIndex.size
      val maxX = linesWithIndex.head._1.length

      def replace(l: List[(String, Int)], replacePos: (Int, Int))
          : List[(String, Int)] = l.map { case (str, y) =>
        if (y == replacePos._2) {
          val newS = str.zipWithIndex.map { case (c, x) =>
            if (x == replacePos._1) '#' else c
          }
          (newS.mkString, y)
        } else (str, y)
      }

      // Bruteforce FTW!
      @tailrec
      def a2Loop(
          y: Int,
          replaceX: Int,
          l: List[(String, Int)],
          loopCount: Int
      ): Int = {
        if (replaceX >= maxX) loopCount
        else {
          val current = charAt(l)(replaceX, y)
          if (current.contains('#'))
            a2Loop(y, replaceX + 1, l, loopCount)
          else {
            val newL = replace(l, (replaceX, y))
            val c = if (loopCircleDect(newL, startPos.get, Up, Nil)) 1 else 0
            a2Loop(y, replaceX + 1, l, loopCount + c)
          }
        }
      }

      def a2 = {
        (0 to maxY).toList.parTraverse(y => IO(a2Loop(y, 0, linesWithIndex, 0)))
      }.map(_.sum)

      IO.println(a1) >> a2.flatMap(IO.println)
    }
    .as(ExitCode.Success)
}
