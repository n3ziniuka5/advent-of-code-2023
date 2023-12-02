package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day2:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day2.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Int =
        lines
            .map { case s"Game $game: $drawLine" =>
                val draws = drawLine.split(";").toList.map(_.trim).map { draw =>
                    draw.split(",").map(_.trim).map { case s"$number $color" =>
                        (number.toInt, color)
                    }
                }
                (game.toInt, draws)
            }
            .filter { (game, draws) =>
                draws.forall { draw =>
                    draw.forall { case (number, color) =>
                        color match {
                            case "red"   => number <= 12
                            case "green" => number <= 13
                            case "blue"  => number <= 14
                        }
                    }
                }
            }
            .map(_._1)
            .sum

    def part2(lines: List[String]): Int =
        lines
            .map { case s"Game $game: $drawLine" =>
                val draws = drawLine.split(";").toList.map(_.trim).map { draw =>
                    draw.split(",").map(_.trim).map { case s"$number $color" =>
                        (number.toInt, color)
                    }
                }
                (game.toInt, draws)
            }
            .map { (game, draws) =>
                val red   = draws.flatten.filter(_._2 == "red").map(_._1).max
                val green = draws.flatten.filter(_._2 == "green").map(_._1).max
                val blue  = draws.flatten.filter(_._2 == "blue").map(_._1).max

                red * green * blue
            }
            .sum
