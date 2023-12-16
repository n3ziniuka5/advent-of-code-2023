package aoc

import aoc.Common.timed

import scala.io.Source

object Day16:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day16.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val points = parse(lines)
        val maxX   = points.keys.map(_.x).max
        val maxY   = points.keys.map(_.y).max
        energized(List((Point(0, 0), Point(-1, 0))), Set.empty, maxX, maxY, points)

    def part2(lines: List[String]): Long =
        val points = parse(lines)
        val maxX   = points.keys.map(_.x).max
        val maxY   = points.keys.map(_.y).max

        val downwards = (0 to maxX).map { x =>
            val point = Point(x, 0)
            List((point, point.up))
        }
        val upwards = (0 to maxX).map { x =>
            val point = Point(x, maxY)
            List((point, point.down))
        }

        val fromLeft = (0 to maxY).map { y =>
            val point = Point(0, y)
            List((point, point.left))
        }

        val fromRight = (0 to maxY).map { y =>
            val point = Point(maxX, y)
            List((point, point.right))
        }

        val allStarts = downwards ++ upwards ++ fromLeft ++ fromRight
        allStarts.map { start =>
            energized(start, Set.empty, maxX, maxY, points)
        }.max

    case class Point(x: Int, y: Int):
        def up: Point    = Point(x, y - 1)
        def down: Point  = Point(x, y + 1)
        def left: Point  = Point(x - 1, y)
        def right: Point = Point(x + 1, y)
        def inBounds(maxX: Int, maxY: Int): Boolean =
            x >= 0 && x <= maxX && y >= 0 && y <= maxY

    def parse(lines: List[String]): Map[Point, Char] =
        lines.zipWithIndex.flatMap { (line, y) =>
            line.zipWithIndex.map { (c, x) =>
                Point(x, y) -> c
            }
        }.toMap

    def energized(
        search: List[(Point, Point)],
        visited: Set[(Point, Point)],
        maxX: Int,
        maxY: Int,
        points: Map[Point, Char]
    ): Int =
        if (search.isEmpty) visited.map(_._1).size
        else if (visited.contains(search.head)) energized(search.tail, visited, maxX, maxY, points)
        else
            val (current, previous) = search.head
            val newSearches = points(current) match
                case '.' =>
                    if (previous == current.up) List((current.down, current))
                    else if (previous == current.down) List((current.up, current))
                    else if (previous == current.left) List((current.right, current))
                    else List((current.left, current))
                case '|' =>
                    if (previous == current.up) List((current.down, current))
                    else if (previous == current.down) List((current.up, current))
                    else
                        List((current.up, current), (current.down, current))
                case '-' =>
                    if (previous == current.left) List((current.right, current))
                    else if (previous == current.right) List((current.left, current))
                    else
                        List((current.left, current), (current.right, current))
                case '\\' =>
                    if (previous == current.up) List((current.right, current))
                    else if (previous == current.down) List((current.left, current))
                    else if (previous == current.right) List((current.up, current))
                    else List((current.down, current))
                case '/' =>
                    if (previous == current.up) List((current.left, current))
                    else if (previous == current.down) List((current.right, current))
                    else if (previous == current.left) List((current.up, current))
                    else List((current.down, current))

            energized(
              newSearches.filterNot(visited.contains).filter(_._1.inBounds(maxX, maxY)) ++ search.tail,
              visited + search.head,
              maxX,
              maxY,
              points
            )
