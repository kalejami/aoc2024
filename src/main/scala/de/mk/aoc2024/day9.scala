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

      //00...
      //111...
      case class Block(idx: Long, xs: List[Long], free: Long) {
        def combine(b2: Block): (Block, Option[Block]) = {
          @tailrec
          def go(toAdd: Block, currentBlock: Block): (Block, Option[Block]) =
            toAdd.xs match {
              case ::(head, next) =>
                val size = 1
                if (size <= currentBlock.free)
                  go(
                    Block(toAdd.idx, next, toAdd.free + size),
                    Block(
                      currentBlock.idx,
                      currentBlock.xs :+ head,
                      currentBlock.free - size
                    )
                  )
                else (currentBlock, Some(toAdd))
              case Nil => (currentBlock, None)
            }
          go(b2, this)
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
                  List.fill(head.toString.toInt)(id),
                  next.toString.toLong
                )
                loop(id + 1, rest, nextBlock :: acc)
              case Nil =>
                val nextBlock =
                  Block(id, List.fill(head.toString.toInt)(id), 0L)
                loop(id + 1, tail, nextBlock :: acc)
            }
          case Nil => acc.reverse
        }

      val str = loop(0L, s, Nil)

      def x(br: List[Block], xs: List[Block], acc: List[Block]): List[Block] =
        br match {
          case ::(head, next) =>
            @tailrec
            def go(rest: Block, it: List[Block], acc: List[Block])
                : List[Block] = it match {
              case ::(ah, at) =>
                if (rest.idx == ah.idx) rest :: acc
                else {
                  val (newAh, newRest) = ah.combine(rest)
                  newRest match {
                    case Some(r) => go(r, at, newAh :: acc)
                    case None    => newAh :: acc
                  }
                }
              case Nil => acc
            }
            if (acc.find(_.idx == head.idx).exists(_.free == 0)) acc
            else {
              val newAcc = go(head, xs, Nil)
              x(
                next,
                xs.map(b => newAcc.find(_.idx == b.idx).getOrElse(b)),
                newAcc
              )
            }
          case Nil => acc
        }

      val blocks = x(str.reverse, str, Nil).reverse

      println(str)
      println(blocks)

      @tailrec
      def sumLoop(
          prev: Option[Block],
          xs: List[Block],
          offset: Long,
          sum: Long
      ): Long = {
        val newOffset = offset + (prev match {
          case Some(v) => v.xs.size + v.free
          case None    => 0
        })

        xs.headOption match {
          case Some(current) =>
            val curSum = current.xs.zipWithIndex.map { case (l, idx) =>
              l * (idx + newOffset)
            }.sum
            sumLoop(Some(current), xs.tail, newOffset, sum + curSum)
          case None => sum
        }

      }

      def a1: Long = sumLoop(None, blocks, 0L, 0L)

      def a2: Long = 0L

      IO.println(a1) >> IO.println(a2)
    }
    .as(ExitCode.Success)
}
