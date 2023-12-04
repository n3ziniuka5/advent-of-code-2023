package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day4:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day4.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Int =
        lines
            .map { case s"Card $n: $winningLine | $yourNumbers" =>
                println(s"parsing $winningLine")
                val a = winningLine.split(" ").flatMap(_.trim.toIntOption).toSet
                val b = yourNumbers.split(" ").flatMap(_.trim.toIntOption).toSet
                (n.trim.toInt, a, b)
            }
            .map { case (n, a, b) =>
                val matchCount = b.count(a.contains)
                if (matchCount > 0) math.pow(2, matchCount - 1).toInt else 0
            }
            .sum

    def part2(lines: List[String]): Int =
        val counts = lines
            .map { case s"Card $n: $winningLine | $yourNumbers" =>
                println(s"parsing $winningLine")
                val a = winningLine.split(" ").flatMap(_.trim.toIntOption).toSet
                val b = yourNumbers.split(" ").flatMap(_.trim.toIntOption).toSet
                (n.trim.toInt, a, b)
            }
            .foldLeft(Map.empty[Int, Int]) { case (acc, (n, a, b)) =>
                val matchCount = b.count(a.contains)
                (1 to matchCount).foldLeft(acc.updated(n, acc.getOrElse(n, 0) + 1)) { case (acc, i) =>
                    acc.updated(n + i, acc.getOrElse(n + i, 0) + acc(n))
                }
            }

        println(counts)

        counts.values.sum
