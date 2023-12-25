package aoc

import aoc.Common.timed

import scala.io.Source

object Day24:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day24.txt").getLines().toList
        timed("Part 1", part1(lines, 200000000000000L, 400000000000000L))
        timed("Part 2", part2(lines))

    case class Hailstone(x: Long, y: Long, z: Long, velocityX: Int, velocityY: Int, velocityZ: Int)

    def parse(lines: List[String]): List[Hailstone] =
        lines.map { case s"$x, $y, $z @ $vx, $vy, $vz" =>
            Hailstone(x.toLong, y.toLong, z.toLong, vx.toInt, vy.toInt, vz.toInt)
        }

    def intersect(h1: Hailstone, h2: Hailstone): Boolean =
        h1.x == h2.x && h1.y == h2.y && h1.z == h2.z

    // aDY * bX + bDX * aDY * bI - adY * aX = aDX * bY + aDX * bDY * bI - aDX * aY
    def findBI(a: Hailstone, b: Hailstone): Option[Double] =
        val top    = a.velocityX * a.y - a.velocityX * b.y - a.velocityY * a.x + a.velocityY * b.x
        val bottom = a.velocityX * b.velocityY - a.velocityY * b.velocityX

        if bottom == 0 then None
        else Some(top.toDouble / bottom)

    def aIForX(a: Hailstone, b: Hailstone, bi: Double): Double =
        (b.x + (b.velocityX * bi) - a.x) / a.velocityX

    def aIForY(a: Hailstone, b: Hailstone, bi: Double): Double =
        (b.y + (b.velocityY * bi) - a.y) / a.velocityY

    def part1(lines: List[String], minCoord: Long, maxCoord: Long): Int =
        val hailstones = parse(lines)
        println(hailstones)

        val a = hailstones(0)
        val b = hailstones(1)

        // aI = (bX + (bDX * bI) - aX) / aDX

        println("using formula aI = (bX + (bDX * bI) - aX) / aDX")
        println("we pick bI = 2")
        println(s"aI for X = ${(b.x + (b.velocityX * 2) - a.x).toDouble / a.velocityX}")
        println(s"aI for Y = ${(b.y + (b.velocityY * 2) - a.y).toDouble / a.velocityY}")

        println(s"solved ${findBI(a, b)}")

        hailstones.combinations(2).count { pair =>
            val a = pair(0)
            val b = pair(1)

            val bi = findBI(a, b)
            println(s"found $bi for $a $b")
            bi match
                case Some(value) =>
                    val aiX = aIForX(a, b, value)
                    val aiY = aIForY(a, b, value)

                    val points = List(
                      a.x + (a.velocityX * aiX),
                      a.y + (a.velocityY * aiY),
                      b.x + (b.velocityX * value),
                      b.y + (b.velocityY * value)
                    )

                    val returning = value >= 0 && aiX >= 0 && points.forall(p => p >= minCoord && p <= maxCoord)

                    println(s"points $points")
                    println(s"returning $returning")
                    println

                    returning
                case None => false
        }

    def part2(lines: List[String]): Int =
        0
