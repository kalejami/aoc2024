package de.mk.aoc2024

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import cats.data.Op

object day3 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Util
      .getInput("input3.txt")
      .use { bs =>
        val lines = bs.getLines().toList

        val input = lines.mkString

        val regex = """mul\(([0-9]{1,3}),([0-9]{1,3})\)""".r
        val instrR = """do\(\)""".r
        val instrDnR = """don\'t\(\)""".r

        val muls = regex.findAllMatchIn(input).toList

        def a1 = muls.map { m =>
          m.group(1).toLong * m.group(2).toLong
        }

        val mulsWithIndex = muls
          .map(m => (m.end, (m.group(1).toLong * m.group(2).toLong)))
          .sortBy(_._1)

        val dosWithIndex =
          instrR.findAllMatchIn(input).toList.map(m => (m.end, m))
        val dontsWithIndex =
          instrDnR.findAllMatchIn(input).toList.map(m => (m.end, m))

        def a2 = mulsWithIndex.mapFilter { case (end, mul) =>
          val donts = dontsWithIndex.filter(_._1 < end).maximumByOption(_._1)
          val dos = dosWithIndex.filter(_._1 < end).maximumByOption(_._1)
          donts match {
            case Some(dn) =>
              dos match {
                case Some(d) => if (d._1 > dn._1) Some(mul) else None
                case None    => None
              }
            case None => Some(mul)
          }
        }

        IO.println(a2.sum)
      }
      .as(ExitCode.Success)

}
