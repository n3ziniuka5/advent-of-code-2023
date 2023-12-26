package aoc

import aoc.Common.timed

import scala.io.Source
import scala.collection.parallel.CollectionConverters._

object Day24:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day24.txt").getLines().toList
        timed("Part 1", part1(lines, 200000000000000L, 400000000000000L))
        timed("Part 2", part2(lines))

    case class Hailstone(x: Long, y: Long, z: Long, velocityX: Long, velocityY: Long, velocityZ: Long):
        def afterIterations(i: Long): Hailstone =
            copy(
              x = x + (velocityX * i),
              y = y + (velocityY * i),
              z = z + (velocityZ * i)
            )

        def distanceTo(other: Hailstone): Double = distanceBetweenHailstones(this, other)

    def parse(lines: List[String]): List[Hailstone] =
        lines.map { case s"$x, $y, $z @ $vx, $vy, $vz" =>
            Hailstone(x.toLong, y.toLong, z.toLong, vx.toInt, vy.toInt, vz.toInt)
        }

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

    def perfectCollision(a: Hailstone, b: Hailstone): Boolean =
        val bi = findBI(a, b)
        bi match
            case Some(value) =>
                val aiX = aIForX(a, b, value)
                val aiY = aIForY(a, b, value)

                val perfectIntegers = value.isWhole && aiX.isWhole && aiY.isWhole
                val allEqual        = value == aiX && value == aiY
                val allPositive     = value >= 0 && aiX >= 0 && aiY >= 0

                val aAfterIterations = a.afterIterations(aiX.toLong)
                val bAfterIterations = b.afterIterations(value.toLong)

                val collide =
                    aAfterIterations.x == bAfterIterations.x && aAfterIterations.y == bAfterIterations.y && aAfterIterations.z == bAfterIterations.z

                val r = perfectIntegers && allEqual && allPositive && collide

                /*if (a == Hailstone(24, 13, 10, -3, 1, 2)) {
                    println(
                      s"!!! colliding with ${b} ${a.afterIterations(aiX.toLong)} ${b.afterIterations(value.toLong)}"
                    )
                    println(
                      s"value $value aiX $aiX aiY $aiY perfectIntegers $perfectIntegers allEqual $allEqual allPositive $allPositive collide $collide"
                    )
                    println(s"returning $r")
                }*/

                r

            case None => false

    def part1(lines: List[String], minCoord: Long, maxCoord: Long): Int =
        val hailstones = parse(lines)
        // println(hailstones)

        // val a = hailstones(0)
        // val b = hailstones(1)

        // aI = (bX + (bDX * bI) - aX) / aDX

        // println("using formula aI = (bX + (bDX * bI) - aX) / aDX")
        // println("we pick bI = 2")
        // println(s"aI for X = ${(b.x + (b.velocityX * 2) - a.x).toDouble / a.velocityX}")
        // println(s"aI for Y = ${(b.y + (b.velocityY * 2) - a.y).toDouble / a.velocityY}")

        // println(s"solved ${findBI(a, b)}")

        hailstones.combinations(2).count { pair =>
            val a = pair(0)
            val b = pair(1)

            val bi = findBI(a, b)
            // println(s"found $bi for $a $b")
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

                    // println(s"points $points")
                    // println(s"returning $returning")
                    // println

                    returning
                case None => false
        }

    def distanceBetweenHailstones(a: Hailstone, b: Hailstone): Double =
        Math.sqrt(
          Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) + Math.pow(a.z - b.z, 2)
        )

    case class SearchState(
        hailstone: Hailstone,
        hailstones: List[Hailstone],
        visited: List[Hailstone],
        iteration: Long,
        distance: Double
    )

    def searchFromHailstone(hailstone: Hailstone, hailstones: List[Hailstone]): Option[List[Hailstone]] =
        /*val initialSearches = hailstones.map { h =>
            SearchState(h,  hailstones.filter(_ != h), )
        }*/

        None

    def part2(lines: List[String]): Long =
        val hailstones = parse(lines)
        /*(0 to 5).foreach { i =>
            println(s"printing distances from 0,0,0 after ${i} nanos")
            val distances = hailstones.map { h =>
                val movedHailstone = h.copy(
                  x = h.x + (h.velocityX * i),
                  y = h.y + (h.velocityY * i),
                  z = h.z + (h.velocityZ * i)
                )
                val distance = Math.sqrt(
                  Math.pow(movedHailstone.x, 2) + Math.pow(movedHailstone.y, 2) + Math.pow(movedHailstone.z, 2)
                )
                (h, distance)
                // println(s"$h distance $distance")
            }

            distances.sortBy(_._2).foreach((h, d) => println(s"$h distance $d"))

            println
        }*/

        println(s"total hailstones ${hailstones.size}")

        /*hailstones.foreach { h =>
            println(s"performing search from ${h}")
            val r = searchFromHailstone(h, hailstones.filterNot(_ == h))
            r match
                case Some(value) =>
                    println("!!! found answer")
                    println(value)
                    throw new RuntimeException("")
                case None => ()
        }*/

        /*val startingPoint = LazyList
            .from(hailstones.combinations(2))
            .flatMap { pair =>
                val a = pair(0)
                val b = pair(1)

                val maxStartIterationGap = 1000
                val pairs = (for {
                    i <- 1 to maxStartIterationGap
                    j <- (i + 1) to (maxStartIterationGap + 1)
                } yield List(
                  ((a.afterIterations(i), i), (b.afterIterations(j), j)),
                  ((b.afterIterations(i), i), (a.afterIterations(j), j))
                )).flatten

                pairs.map { pair =>
                    val ((a, aI), (b, bI)) = pair

                    val gapX = b.x - a.x
                    val gapY = b.y - a.y
                    val gapZ = b.z - a.z

                    val dX = gapX / (bI - aI)
                    val dY = gapY / (bI - aI)
                    val dZ = gapZ / (bI - aI)

                    val shouldMark = a == Hailstone(21, 14, 12, 1, -5, -3) && b == Hailstone(15, 16, 16, -1, -1,
                      -2) && aI == 1 && bI == 3

                    val marker = if shouldMark then "!!! " else ""

                    val startingPoint = Hailstone(a.x - (dX * aI), a.y - (dY * aI), a.z - (dZ * aI), dX, dY, dZ)

                    // println(s"$marker first hitting ${a} at ${aI} and ${b} at ${bI}. Starting at ${startingPoint}")

                    startingPoint
                }

            }
            .find { h =>
                hailstones.forall(h1 => perfectCollision(h, h1))
            }
            .get*/

        val seenStartingPoints = collection.mutable.Set.empty[Hailstone]
        val startingPoint = LazyList
            .from(hailstones)
            .combinations(2)
            .toList
            // .par
            .find { pair =>
                val a = pair(0)
                val b = pair(1)

                val maxStartIterationGap = 1000
                val pairs = (for {
                    i <- 1 to maxStartIterationGap
                    j <- (i + 1) to (maxStartIterationGap + 1)
                } yield List(
                  ((a.afterIterations(i), i), (b.afterIterations(j), j)),
                  ((b.afterIterations(i), i), (a.afterIterations(j), j))
                )).flatten

                val startingPoints = pairs.map { pair =>
                    val ((a, aI), (b, bI)) = pair

                    val gapX = b.x - a.x
                    val gapY = b.y - a.y
                    val gapZ = b.z - a.z

                    val dX = gapX / (bI - aI)
                    val dY = gapY / (bI - aI)
                    val dZ = gapZ / (bI - aI)

                    /*val shouldMark = a == Hailstone(21, 14, 12, 1, -5, -3) && b == Hailstone(15, 16, 16, -1, -1,
                      -2) && aI == 1 && bI == 3

                    val marker = if shouldMark then "!!! " else ""*/

                    val startingPoint = Hailstone(a.x - (dX * aI), a.y - (dY * aI), a.z - (dZ * aI), dX, dY, dZ)

                    // println(s"$marker first hitting ${a} at ${aI} and ${b} at ${bI}. Starting at ${startingPoint}")

                    startingPoint
                }

//                println(startingPoints)
//                println
                startingPoints.filterNot(seenStartingPoints.contains).exists { h =>
                    val r = hailstones.forall(h1 => perfectCollision(h, h1))
                    if (r) println(s"found $h, answer is ${h.x + h.y + h.z}")
                    else seenStartingPoints.add(h)
                    r
                }

            }
            .get

        0

//        startingPoint.x + startingPoint.y + startingPoint.z
