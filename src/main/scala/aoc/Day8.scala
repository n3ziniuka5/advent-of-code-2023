package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day8:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day8.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Int =
        val allInstructions = lines.head.toList

        val destinations = lines
            .drop(2)
            .map { case s"$src = ($left, $right)" =>
                src -> (left, right)
            }
            .toMap

        def loop(instructions: List[Char], currentPos: String, steps: Int): Int =
            if currentPos == "ZZZ" then steps
            else if instructions.isEmpty then loop(allInstructions, currentPos, steps)
            else
                val instruction = instructions.head
                val nextPos     = if (instruction == 'L') destinations(currentPos)._1 else destinations(currentPos)._2
                loop(instructions.tail, nextPos, steps + 1)

        loop(allInstructions, "AAA", 0)

    def part2(lines: List[String]): Long =
        val allInstructions                       = lines.head.toVector
        val instructionLoop                       = allInstructions.size
        def instructionsRepeating: LazyList[Char] = LazyList.from(allInstructions) lazyAppendedAll instructionsRepeating

        val destinations = lines
            .drop(2)
            .map { case s"$src = ($left, $right)" =>
                src -> (left, right)
            }
            .toMap

        val startingPoints = destinations.filter(_._1.endsWith("A")).keySet
        println(s"starting points: $startingPoints")

        println(destinations)

        @tailrec
        def loop(instructions: LazyList[Char], currentPos: String, steps: Long): (String, Long) =
            if currentPos.endsWith("Z") && steps != 0 then (currentPos, steps)
            else
                val instruction = instructions.head
                val nextPos     = if (instruction == 'L') destinations(currentPos)._1 else destinations(currentPos)._2
                loop(instructions.tail, nextPos, steps + 1)

        @tailrec
        def loop2(
            currentSteps: Map[String, (Long, LazyList[Char])],
            cache: Map[(String, Int), (String, Long)],
        ): Long =
            println(s"current steps: ${currentSteps.map(a => (a._1, a._2._1))}")
            val (atEnd, notAtEnd) = currentSteps.partition(_._1.endsWith("Z"))
            if notAtEnd.isEmpty then
                if atEnd.values.map(_._1).toList.distinct.sizeIs == 1 then atEnd.head._2._1
                else
                    val (pos, (stepsSoFar, instructions)) = currentSteps.minBy(_._2._1)

                    cache.get((pos, instructions.take(instructionLoop).toList.hashCode())) match {
                        case Some((dest, additionalSteps)) =>
                            println(s"Cache hit! pos: $pos, stepsSoFar: $stepsSoFar, additionalSteps: $additionalSteps")
                            val newMap = currentSteps - pos + (dest -> (stepsSoFar + additionalSteps, instructions.drop(
                              additionalSteps.toInt
                            )))
                            0
                        // loop2(newMap, cache)
                        case None =>
                            println("not in cache")
                            val (dest, additionalSteps) = loop(instructions, pos, 0)
                            val newMap = currentSteps - pos + (dest -> (stepsSoFar + additionalSteps, instructions.drop(
                              additionalSteps.toInt
                            )))

                            val stepsForEach = atEnd.map { case (pos, (stepsSoFar, instructions)) =>
                                val (dest, additionalSteps) = loop(instructions, pos, 0)
                                println(s"$pos $dest $additionalSteps")
                                stepsSoFar
                            }

                            def gcd(a: Long, b: Long): Long = {
                                if (b == 0) a else gcd(b, a % b)
                            }

                            def lcm(a: Long, b: Long): Long = {
                                (a / gcd(a, b)) * b
                            }

                            stepsForEach.reduceLeft(lcm(_, _))

                        // calculate lcd of stepsForEach

                        // println(s"current steps: ${newMap.map(a => (a._1, a._2._1))}")

                        /*loop2(
                              newMap,
                              cache + ((
                                pos,
                                instructions.take(instructionLoop).toList.hashCode()
                              ) -> (dest, additionalSteps))
                            )*/
                    }
            else
                println(s"not at end: ${notAtEnd.size}")
                val newNotAtEnd = notAtEnd.map { case (pos, (stepsSoFar, instructions)) =>
                    val (dest, additionalSteps) = loop(instructions, pos, 0)
                    dest -> (stepsSoFar + additionalSteps, instructions.drop(additionalSteps.toInt))
                }
                println(s"new not at end: ${newNotAtEnd.size}")
                loop2(newNotAtEnd ++ atEnd, cache)

        /*loop(allInstructions, "AAA", 0)*/
        val a = startingPoints.toList.map { point =>
            val instructions: LazyList[Char] = instructionsRepeating
            val result                       = point -> (0L, instructions)
            result
            /*println("got instructions")
            ()*/
        }.toMap

        loop2(a, Map.empty)
