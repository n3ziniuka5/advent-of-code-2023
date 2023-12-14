package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day14:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day14.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Point(x: Int, y: Int)

    def parse(lines: List[String]): Map[Point, Char] =
        lines.zipWithIndex.flatMap { (line, y) =>
            line.zipWithIndex.map { (char, x) =>
                Point(x, y) -> char
            }
        }.toMap

    def visualize(map: Map[Point, Char]): Unit =
        val visual = (for {
            y <- 0 to map.keys.maxBy(_.y).y
            x <- 0 to map.keys.maxBy(_.x).x
        } yield {
            val pos = Point(x, y)
            map(pos)
        }).grouped(map.keys.maxBy(_.x).x + 1).map(_.mkString).mkString("\n")

        println(visual)

    def slideRow(row: List[Char]): List[Char] =
        if (row.isEmpty) row
        else
            val dotsAndRoundRocks = row.takeWhile(_ != '#')
            val cubeRocks         = row.drop(dotsAndRoundRocks.length).takeWhile(_ == '#')

            val roundRocks = dotsAndRoundRocks.count(_ == 'O')

            val sorted =
                List.fill(roundRocks)('O') ++ List.fill(dotsAndRoundRocks.length - roundRocks)('.') ++ cubeRocks

            sorted ++ slideRow(row.drop(sorted.length))

    def tiltUp(map: Map[Point, Char]): Map[Point, Char] =
        // println("TILTING UP")
        // visualize(map)
        // println()

        val maxX = map.keys.maxBy(_.x).x
        val maxY = map.keys.maxBy(_.y).y
        val rows = (0 to maxX).map { x =>
            val row = (0 to maxY).map { y =>
                map(Point(x, y))
            }.toList
            slideRow(row)
        }.toList

        val newMap = rows.zipWithIndex.flatMap { (row, x) =>
            row.zipWithIndex.map { (char, y) =>
                Point(x, y) -> char
            }
        }.toMap

        // visualize(newMap)
        // println()

        newMap

    def tiltDown(map: Map[Point, Char]): Map[Point, Char] =
        // println("TILTING DOWN")
        // visualize(map)
        // println()

        val maxX = map.keys.maxBy(_.x).x
        val maxY = map.keys.maxBy(_.y).y
        val rows = (0 to maxX).map { x =>
            val row = (maxY to 0 by -1).map { y =>
                map(Point(x, y))
            }.toList
            slideRow(row).reverse
        }.toList

        val newMap = rows.zipWithIndex.flatMap { (row, x) =>
            row.zipWithIndex.map { (char, y) =>
                Point(x, y) -> char
            }
        }.toMap

        // visualize(newMap)
        // println()
        newMap

    def tiltLeft(map: Map[Point, Char]): Map[Point, Char] =
        // println("TILTING LEFT")
        // visualize(map)
        // println()

        val maxX = map.keys.maxBy(_.x).x
        val maxY = map.keys.maxBy(_.y).y
        val rows = (0 to maxY).map { y =>
            val row = (0 to maxX).map { x =>
                map(Point(x, y))
            }.toList
            slideRow(row)
        }.toList

        val newMap = rows.zipWithIndex.flatMap { (row, y) =>
            row.zipWithIndex.map { (char, x) =>
                Point(x, y) -> char
            }
        }.toMap

        // visualize(newMap)
        // println()

        newMap

    def tiltRight(map: Map[Point, Char]): Map[Point, Char] =
        val maxX = map.keys.maxBy(_.x).x
        val maxY = map.keys.maxBy(_.y).y
        val rows = (0 to maxY).map { y =>
            val row = (maxX to 0 by -1).map { x =>
                map(Point(x, y))
            }.toList
            slideRow(row).reverse
        }.toList

        rows.zipWithIndex.flatMap { (row, y) =>
            row.zipWithIndex.map { (char, x) =>
                Point(x, y) -> char
            }
        }.toMap

    /*@tailrec
    def tiltUp(map: Map[Point, Char]): Map[Point, Char] =
        val newMap = (for {
            y <- 0 to map.keys.maxBy(_.y).y
            x <- 0 to map.keys.maxBy(_.x).x
        } yield {
            val point = Point(x, y)
            val above = point.copy(y = y - 1)

            if y == 0 then List(point -> map(point))
            else if (map(point) == 'O' && map(above) == '.') {
                List(above -> 'O', point -> '.')
            } else {
                List(point -> map(point))
            }
        }).flatten.toMap

        if (newMap != map) tiltUp(newMap) else newMap*/

    /*def tiltDown(map: Map[Point, Char]): Map[Point, Char] =
        val maxY = map.keys.maxBy(_.y).y
        val newMap = (for {
            y <- 0 to maxY
            x <- 0 to map.keys.maxBy(_.x).x
        } yield {
            val point = Point(x, y)
            val below = point.copy(y = y + 1)

            if y == maxY then List(point -> map(point))
            else if (map(point) == 'O' && map(below) == '.') {
                List(below -> 'O', point -> '.')
            } else {
                List(point -> map(point))
            }
        }).flatten.toMap

        if (newMap != map) tiltDown(newMap) else newMap*/

    def cycle(map: Map[Point, Char]): Map[Point, Char] =
        val afterUp   = tiltUp(map)
        val afterLeft = tiltLeft(afterUp)
        val afterDown = tiltDown(afterLeft)
        tiltRight(afterDown)

    val cache = collection.mutable.Map.empty[Map[Point, Char], Long]

    @tailrec
    def cycleUntil(map: Map[Point, Char], n: Long, target: Long): Map[Point, Char] =
        if n == target then map
        else
            val newMap = cycle(map)

            if (cache.contains(newMap))
                println(s"CYCLE DETECTED at ${cache(newMap)}}")
                val lastSeenAt = cache(newMap)
                val cycleSize  = n - lastSeenAt

                (target - n - 1) % cycleSize match
                    case 0 => newMap
                    case remainder =>
                        cache.clear()
                        println(s"REMAINDER is $remainder")
                        cycleUntil(newMap, 0, remainder)
            else
                cache.update(newMap, n)
                cycleUntil(newMap, n + 1, target)

    def northLoad(map: Map[Point, Char]): Long =
        val maxY = map.keys.maxBy(_.y).y
        (for {
            y <- 0 to map.keys.maxBy(_.y).y
            x <- 0 to map.keys.maxBy(_.x).x
        } yield {
            val point = Point(x, y)
            if (map(point) == 'O') (maxY + 1) - y
            else 0
        }).sum

    def part1(lines: List[String]): Long =
        val map    = parse(lines)
        val newMap = tiltUp(map)
        northLoad(newMap)

    def part2(lines: List[String]): Long =
        val map    = parse(lines)
        val newMap = cycleUntil(map, 0, 1000000000)

        /*println("CYCLE 1")
        visualize(newMap)
        println()*/

        northLoad(newMap)
