package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.annotation.tailrec

object day4 extends IOApp {
  private val xmas = "XMAS".r
  def run(args: List[String]): IO[ExitCode] =
    Util
      .getInput("input4.txt")
      .use { bs =>
        val lines = bs.getLines().toList

        def count(xs: List[String]) = xs.map { s =>
          xmas.findAllMatchIn(s).size + xmas.findAllIn(s.reverse).size
        }.sum

        val horizontal = count(lines)

        @tailrec
        def vloop(index: Int, vLines: List[String]): List[String] =
          if (index >= lines.head.length()) vLines.reverse
          else {
            val x = lines.map(_.charAt(index)).mkString
            vloop(index + 1, x :: vLines)
          }

        val verts = vloop(0, Nil)

        val vertikal = count(verts)

        def diag(xs: Array[String]): List[String] = {
          val n = xs.length
          val d1lr =
            for (k <- 0 until n)
              yield (0 to k).map(i => xs(i)(k - i)).mkString
          val d2lr =
            for (k <- 1 until n)
              yield (0 until (n - k))
                .map(i => xs(k + i)(n - 1 - i))
                .mkString
          val d1rl =
            for (k <- 0 until n)
              yield (0 to k).map(i => xs(i)(n - 1 - k + i)).mkString
          val d2rl =
            for (k <- 1 until n)
              yield (0 until (n - k)).map(i => xs(k + i)(i)).mkString
          (d1lr ++ d2lr ++ d1rl ++ d2rl).toList
        }

        val diags = diag(lines.toArray)

        val diagonal = count(diags)

        def a1: Int = horizontal + vertikal + diagonal

        val linesWithIndex = lines.zipWithIndex

        def getChar(x: Int, y: Int) = linesWithIndex
          .find(_._2 == y)
          .flatMap(_._1.zipWithIndex.toList.find(_._2 == x).map(_._1))

        val maxY = lines.size
        val maxX = lines.head.length()

        @tailrec
        def a2Loop(x: Int, y: Int, count: Int): Int =
          if (y >= maxY) count
          else if (x >= maxX) a2Loop(0, y + 1, count)
          else {
            getChar(x, y) match {
              case Some(c) =>
                if (c == 'A') {
                  val ulo = getChar(x - 1, y - 1)
                  val uro = getChar(x + 1, y - 1)
                  val dlo = getChar(x - 1, y + 1)
                  val dro = getChar(x + 1, y + 1)

                  (ulo, uro, dlo, dro).tupled match {
                    case Some((ul, ur, dl, dr)) =>
                      val uldrIsfine =
                        ul == 'M' && dr == 'S' || ul == 'S' && dr == 'M'
                      val urdlIsFine =
                        ur == 'M' && dl == 'S' || ur == 'S' && dl == 'M'
                      if (uldrIsfine && urdlIsFine) a2Loop(x + 1, y, count + 1)
                      else a2Loop(x + 1, y, count)
                    case None => a2Loop(x + 1, y, count)
                  }
                } else a2Loop(x + 1, y, count)
              case None => count
            }
          }

        def a2: Int = a2Loop(0, 0, 0)

        IO.println(a1) >> IO.println(a2)
      }
      .as(ExitCode.Success)

}
