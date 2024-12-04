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

        def count(xs: List[String]) = {
          xs.map { s =>
            xmas.findAllMatchIn(s).size + xmas.findAllIn(s.reverse).size
          }.sum
        }

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

        def diag(matrix: Array[String]): Array[String] = {
          val n = matrix.length
          val diagonals = scala.collection.mutable.ArrayBuffer[String]()

          // Erste Hälfte der Diagonalen von links oben nach rechts unten
          for (k <- 0 until n) {
            val diag1 = (0 to k).map(i => matrix(i)(k - i)).mkString
            diagonals.append(diag1)
          }

          // Zweite Hälfte der Diagonalen von links oben nach rechts unten
          for (k <- 1 until n) {
            val diag2 =
              (0 until (n - k)).map(i => matrix(k + i)(n - 1 - i)).mkString
            diagonals.append(diag2)
          }

          // Erste Hälfte der Diagonalen von rechts oben nach links unten
          for (k <- 0 until n) {
            val diag3 = (0 to k).map(i => matrix(i)(n - 1 - k + i)).mkString
            diagonals.append(diag3)
          }

          // Zweite Hälfte der Diagonalen von rechts oben nach links unten
          for (k <- 1 until n) {
            val diag4 = (0 until (n - k)).map(i => matrix(k + i)(i)).mkString
            diagonals.append(diag4)
          }

          diagonals.toArray
        }

        val diags = diag(lines.toArray).toList

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

        IO.println(a2)
      }
      .as(ExitCode.Success)

}
