package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.syntax.all._

import scala.annotation.tailrec

object day9 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input9.txt")
    .use { bs =>
      val lines = bs.getLines().toList
      val s = lines.mkString.toList
      //val s = "2333133121414131402".toList

      // 00...
      // 111...
      case class Block(idx: Long, xs: List[Option[Long]]) {
        def free: Int = emptyBlocks().size
        def emptyBlocks(): List[Int] =
          xs.zipWithIndex.filter(_._1.isEmpty).map(_._2)

        def allocated: List[Int] =
          xs.groupBy(identity).filter(_._1.nonEmpty).toList.map(_._2.size)

        def combineA2(b2: Block): (Block, Option[Block]) = {
          val grouped = b2.xs.filter(_.nonEmpty).groupBy(identity)

          grouped.toList.findLast(_._2.size <= this.free) match {
            case Some((i, blocks)) =>
              val size = blocks.size
              val newBlock2 = b2.copy(
                xs = b2.xs.map(x => if (x == i) None else x)
              )

              val newFree = free - size

              (
                Block(
                  idx,
                  xs.filter(_.nonEmpty) ++ blocks ++ List.fill(newFree)(
                    None
                  )
                ),
                Option(newBlock2)
              )
            case None => (this, Some(b2))
          }
        }
      }

      @tailrec
      def loop(id: Long, xs: List[Char], acc: List[Block]): List[Block] =
        xs match {
          case ::(head, tail) =>
            tail match {
              case ::(next, rest) =>
                val nextBlock = Block(
                  id,
                  List.fill(head.toString.toInt)(Some(id)) ++ List.fill(
                    next.toString.toInt
                  )(None)
                )
                loop(id + 1, rest, nextBlock :: acc)
              case Nil =>
                val nextBlock =
                  Block(id, List.fill(head.toString.toInt)(Some(id)))
                loop(id + 1, tail, nextBlock :: acc)
            }
          case Nil => acc.reverse
        }

      val str = loop(0L, s, Nil)

      @tailrec
      def sumLoop(
          prev: Option[Block],
          xs: List[Block],
          offset: Long,
          sum: Long
      ): Long = {
        val newOffset = offset + (prev match {
          case Some(v) => v.xs.size
          case None    => 0
        })

        xs.headOption match {
          case Some(current) =>
            val curSum = current.xs.zipWithIndex.map { case (l, idx) =>
              l.getOrElse(0L) * (idx + newOffset)
            }.sum
            sumLoop(Some(current), xs.tail, newOffset, sum + curSum)
          case None => sum
        }

      }

      // def a1: Long = sumLoop(None, blocks, 0L, 0L)

      @tailrec
      def a2L(xs: List[Block], acc: List[Block]): List[Block] = xs match {
        case ::(head, next) =>
          val maybeBlock = acc.find(_.idx == head.idx)
          val b = {
            maybeBlock
              .flatMap(ho =>
                acc
                  .find(a =>
                    ho.allocated.exists(as => a.free >= as) && a.idx < ho.idx
                  )
                  .map {
                    _.combineA2(ho)
                  }
              )
          }
          b match {
            case Some(f) =>
              val newAcc: List[Block] = acc.mapFilter { a =>
                if (a.idx == f._1.idx) Some(f._1)
                else if (f._2.exists(_.idx == a.idx)) f._2
                else if (a.idx == head.idx) None
                else Some(a)
              }
              a2L(next, newAcc)
            case None => a2L(next, acc)
          }
        case Nil => acc
      }

      val a2Blocks = a2L(str.reverse, str)

      println(a2Blocks)

      def a2: Long = sumLoop(None, a2Blocks, 0L, 0L)

      IO.println(a2)
    }
    .as(ExitCode.Success)
}
