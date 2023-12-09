package aoc

import aoc.Common.timed

import scala.io.Source

object Day9:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day9.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        def solve(seq: Vector[Long]): Long =
            if (seq.forall(_ == 0)) 0
            else {
                val diffs = seq.sliding(2, 1).map { a => a(1) - a(0) }.toVector
                seq.last + solve(diffs)
            }

        lines.map(_.split(" ").map(_.toLong).toVector).map(solve).sum

    def part2(lines: List[String]): Long =
        def solve(seq: Vector[Long]): Long =
            if (seq.forall(_ == 0)) 0
            else {
                val diffs = seq.sliding(2, 1).map { a => a(1) - a(0) }.toVector
                seq.head - solve(diffs)
            }

        lines.map(_.split(" ").map(_.toLong).toVector).map(solve).sum
