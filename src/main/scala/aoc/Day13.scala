package aoc

import aoc.Common.timed

import scala.io.Source

object Day13:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day13.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Point(x: Int, y: Int)

    def parse(lines: List[String], maps: List[Map[Point, Char]]): List[Map[Point, Char]] =
        val nonEmpty = lines.dropWhile(_.isEmpty).takeWhile(_.nonEmpty)
        if (nonEmpty.isEmpty) maps.reverse
        else
            val newRes = nonEmpty.zipWithIndex.flatMap { (line, y) =>
                line.zipWithIndex.map { (char, x) =>
                    Point(x, y) -> char
                }
            }.toMap
            parse(lines.drop(nonEmpty.length + 1), newRes +: maps)

    def part1(lines: List[String]): Long =
        val parsed = parse(lines, Nil)
        println(parsed.size)
        println(parsed.head)

        val pathVisual = (for {
            y <- 0 to parsed.head.keys.maxBy(_.y).y
            x <- 0 to parsed.head.keys.maxBy(_.x).x
        } yield {
            val pos = Point(x, y)
            parsed.head(pos)
        }).grouped(parsed.head.keys.maxBy(_.x).x + 1).map(_.mkString).mkString("\n")

        println()
        println(pathVisual)
        println()

        val pathVisual2 = (for {
            y <- 0 to parsed(1).keys.maxBy(_.y).y
            x <- 0 to parsed(1).keys.maxBy(_.x).x
        } yield {
            val pos = Point(x, y)
            parsed(1)(pos)
        }).grouped(parsed(1).keys.maxBy(_.x).x + 1).map(_.mkString).mkString("\n")

        println()
        println(pathVisual2)
        println()

        parsed.map { map =>
            val maxX = map.keys.maxBy(_.x).x
            val maxY = map.keys.maxBy(_.y).y

            val verticalLine = (1 to maxX).find { mirrorAt =>
                val toTheLeft  = ((mirrorAt - 1) to 0 by -1).toList
                val toTheRight = (mirrorAt to maxX).toList

                val zipped = toTheLeft.zip(toTheRight)

                (0 to maxY).forall { y =>
                    zipped.forall((left, right) => map(Point(left, y)) == map(Point(right, y)))
                }
            }

            verticalLine match
                case Some(value) =>
                    println(s"found vertical line at $value")
                    value
                case None =>
                    val horizontalLine = (1 to maxY).find { mirrorAt =>
                        val up   = ((mirrorAt - 1) to 0 by -1).toList
                        val down = (mirrorAt to maxY).toList

                        val zipped = up.zip(down)

                        (0 to maxX).forall { x =>
                            zipped.forall((left, right) => map(Point(x, left)) == map(Point(x, right)))
                        }
                    }.get

                    println(s"found horizontal line at $horizontalLine")

                    horizontalLine * 100

        }.sum

    def part2(lines: List[String]): Long =
        val parsed = parse(lines, Nil)

        println(parsed.size)
        println(parsed.head)

        val pathVisual = (for {
            y <- 0 to parsed.head.keys.maxBy(_.y).y
            x <- 0 to parsed.head.keys.maxBy(_.x).x
        } yield {
            val pos = Point(x, y)
            parsed.head(pos)
        }).grouped(parsed.head.keys.maxBy(_.x).x + 1).map(_.mkString).mkString("\n")

        println()
        println(pathVisual)
        println()

        val pathVisual2 = (for {
            y <- 0 to parsed(1).keys.maxBy(_.y).y
            x <- 0 to parsed(1).keys.maxBy(_.x).x
        } yield {
            val pos = Point(x, y)
            parsed(1)(pos)
        }).grouped(parsed(1).keys.maxBy(_.x).x + 1).map(_.mkString).mkString("\n")

        println()
        println(pathVisual2)
        println()

        parsed.map { map =>
            val maxX = map.keys.maxBy(_.x).x
            val maxY = map.keys.maxBy(_.y).y

            val verticalLine = (1 to maxX).find { mirrorAt =>
                val toTheLeft  = ((mirrorAt - 1) to 0 by -1).toList
                val toTheRight = (mirrorAt to maxX).toList

                val zipped = toTheLeft.zip(toTheRight)

                val nonMatch = (0 to maxY).map { y =>
                    zipped.count((left, right) => map(Point(left, y)) != map(Point(right, y)))
                }.sum
                nonMatch == 1
            }

            verticalLine match
                case Some(value) =>
                    println(s"found vertical line at $value")
                    value
                case None =>
                    val horizontalLine = (1 to maxY).find { mirrorAt =>
                        val up   = ((mirrorAt - 1) to 0 by -1).toList
                        val down = (mirrorAt to maxY).toList

                        val zipped = up.zip(down)

                        val nonMatch = (0 to maxX).map { x =>
                            zipped.count((left, right) => map(Point(x, left)) != map(Point(x, right)))
                        }.sum
                        nonMatch == 1
                    }.get

                    println(s"found horizontal line at $horizontalLine")

                    horizontalLine * 100

        }.sum
