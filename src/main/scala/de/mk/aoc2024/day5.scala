package de.mk.aoc2024

import cats.effect.{ExitCode, IO, IOApp}

import scala.annotation.tailrec

object day5 extends IOApp {
  def run(args: List[String]): IO[ExitCode] = Util
    .getInput("input5.txt")
    .use { bs =>
      val lines = bs.getLines().toList

      @tailrec
      def pLoop(
          l: List[String],
          order: List[(Long, Long)],
          queue: List[List[Long]]
      ): (List[(Long, Long)], List[List[Long]]) = l match {
        case ::(head, next) =>
          val xy = head.split('|')
          val queueS = head.split(',')
          if (xy.length > 1) {
            pLoop(next, (xy(0).toLong, xy(1).toLong) :: order, queue)
          } else if (queueS.length > 1) {
            val qs = queueS.toList.map(_.toLong)
            pLoop(next, order, qs :: queue)
          } else pLoop(next, order, queue)
        case Nil => (order, queue)
      }

      val (order, queue) = pLoop(lines, Nil, Nil)

      val orderM = order.groupMap(_._1)(_._2)

      // Sortiert die Elemente in der Liste nach Anzahl moeglicher
      // gueltiger Nachfolger. Dies muss die richtige Reihenfolge
      // fuer die Liste ergeben
      def sort(qs: List[Long]) =
        qs.map { q =>
          val relevant = orderM.getOrElse(q, Nil).filter(qs.contains)
          q -> relevant
        }.sortBy(_._2.size)
          .reverse
          .map(_._1)

      def a1: Long =
        queue.map { q => if (q == sort(q)) q(q.size / 2) else 0L }.sum

      def a2: Long = queue.map { q =>
        val qs = sort(q)
        if (qs != q) qs(q.size / 2) else 0L
      }.sum

      IO.println(a1) >> IO.println(a2)
    }
    .as(ExitCode.Success)
}
