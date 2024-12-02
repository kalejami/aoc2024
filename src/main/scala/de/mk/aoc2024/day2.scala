package de.mk.aoc2024

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import cats.data.Op

object day2 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Util
      .getInput("input2.txt")
      .use { bs =>
        val lines = bs.getLines
        val nums = lines.toList.map { line =>
          val split = line.split(" ")
          split.toList.map(_.toLong)
        }

        def a1 = nums.map { xs =>
          val zipped = xs.zip(xs.tail)

          def loop(xs: List[(Long, Long)], vzO: Option[Int], safe: Boolean)
              : Boolean =
            xs match {
              case head :: next =>
                if (!safe) safe
                else {
                  val dist = head._2 - head._1
                  val vz = if (dist == 0) 0 else if (dist > 0) 1 else -1
                  if (vzO.nonEmpty) {
                    if (vzO.contains(vz)) {
                      if (dist == 0 || (dist * vz) > 3) false
                      else loop(next, vzO, true)
                    } else false
                  } else {
                    if (dist == 0 || (dist * vz) > 3) false
                    else loop(next, Some(vz), true)
                  }
                }
              case Nil => safe
            }
          loop(zipped, None, true)
        }

        def a2 = nums.map { xs =>
          def loop(xs: List[(Long, Long)], vzO: Option[Int], safe: Boolean)
              : Boolean =
            xs match {
              case head :: next =>
                if (!safe) safe
                else {
                  val dist = head._2 - head._1
                  val vz = if (dist == 0) 0 else if (dist > 0) 1 else -1
                  if (vzO.nonEmpty) {
                    if (vzO.contains(vz)) {
                      if (dist == 0 || (dist * vz) > 3) false
                      else loop(next, vzO, true)
                    } else false
                  } else {
                    if (dist == 0 || (dist * vz) > 3) false
                    else loop(next, Some(vz), true)
                  }
                }
              case Nil => safe
            }

          def loop2(l: List[Long], currentIndex: Int): Boolean = {
            val zipped = l.zip(l.tail)

            if (loop(zipped, None, true)) true
            else {
              if (currentIndex > xs.size) false
              else {
                val filtered = xs.zipWithIndex.mapFilter { case (x, i) =>
                  if (i == currentIndex) Option.empty[Long]
                  else Some(x)
                }
                loop2(filtered, currentIndex + 1)
              }
            }
          }

          loop2(xs, 0)
        }

        IO.println(a2.count(_ == true))
      }
      .as(ExitCode.Success)

}
