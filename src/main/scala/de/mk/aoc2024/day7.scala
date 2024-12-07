package de.mk.aoc2024

import cats.syntax.all._
import cats.effect.{ExitCode, IO, IOApp}

import scala.annotation.tailrec

object day7 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input7.txt")
    .use { bs =>
      val lines = bs.getLines().toList

      val eqs = lines.map { l =>
        val s1 = l.split(":")
        (s1(0).toLong, s1(1).split(" ").toList.mapFilter(_.toLongOption))
      }

      sealed trait Op {
        def calc: Long = this match {
          case Num(l)       => l
          case Mul(o1, o2)  => o1.calc * o2.calc
          case Add(o1, o2)  => o1.calc + o2.calc
          case Conc(o1, o2) => s"${o1.calc}${o2.calc}".toLong
        }
      }

      object Op {
        @tailrec
        private def toOpLoop(it: List[Long], acc: List[Op]): List[Op] =
          it match {
            case ::(head, next) =>
              val l = Num(head)
              val newAcc =
                if (acc.isEmpty) List(l)
                else acc.flatMap(e => List(Add(e, l), Mul(e, l), Conc(e, l)))
              toOpLoop(next, newAcc)
            case Nil => acc
          }

        def toOp(list: List[Long]): List[Op] = toOpLoop(list, Nil)
      }
      case class Num(l: Long) extends Op
      case class Mul(o1: Op, o2: Op) extends Op
      case class Add(o1: Op, o2: Op) extends Op
      case class Conc(o1: Op, o2: Op) extends Op

      def res: Long = {
        eqs
          .mapFilter { case (l, longs) =>
            Op.toOp(longs).find(_.calc == l)
          }
          .map(_.calc)
          .sum
      }

      IO.println(res)
    }
    .as(ExitCode.Success)
}
