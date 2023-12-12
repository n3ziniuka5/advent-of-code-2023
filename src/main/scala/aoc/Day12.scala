package aoc

import aoc.Common.timed
import zio.prelude.ZSet
import scala.collection.parallel.CollectionConverters._

import scala.io.Source

object Day12:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day12.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    // returns a list of possible string sizes
    def dfs(lines: List[(String, Int, Int)], target: Int, result: ZSet[Int, Int]): ZSet[Int, Int] =
        if (lines.isEmpty) result
        else
            val (line, discardedLength, currentBroken) = lines.head
            val mustBeBroken                           = line.takeWhile(_ == '#')
            if mustBeBroken.nonEmpty then
                // println("looping 1")
                val newBroken = currentBroken + mustBeBroken.length
                if newBroken > target then dfs(lines.tail, target, result)
                else if newBroken == target then
                    dfs(lines.tail, target, result.combine(ZSet(discardedLength + mustBeBroken.length + 1)))
                else
                    val newLine = (line.drop(mustBeBroken.length), discardedLength + mustBeBroken.length, newBroken)
                    dfs(
                      newLine +: lines.tail,
                      target,
                      result
                    )
            else if currentBroken == target then
                // println("looping 3")
                dfs(lines.tail, target, result.combine(ZSet(discardedLength + 1)))
            else if line.isEmpty then
                // println("looping 4")
                dfs(lines.tail, target, result)
            else if line.head == '.' && currentBroken > 0 then dfs(lines.tail, target, result)
            else if line.head == '.' then
                val dotsDropped = line.dropWhile(_ == '.')
                val newLine     = (dotsDropped, discardedLength + (line.length - dotsDropped.length), currentBroken)
                dfs(newLine +: lines.tail, target, result)
            else if currentBroken == 0 then
                // println("looping 2")
                val newLineEmpty = (line.tail, discardedLength + 1, 0)
                val newLineFull  = (line.tail, discardedLength + 1, 1)
                dfs(List(newLineEmpty, newLineFull) ++ lines.tail, target, result)
            else
                // println("looping 5")
                val newLine = (line.tail, discardedLength + 1, currentBroken + 1)
                dfs(newLine +: lines.tail, target, result)

    val cache  = collection.concurrent.TrieMap.empty[(String, Int), Map[Int, Int]]
    val cache2 = collection.concurrent.TrieMap.empty[(String, List[Int]), Option[Long]]

    def waysToSolve(line: String, segments: List[Int]): Option[Long] =
        if line.isEmpty && segments.isEmpty then Some(1)
        else if line.isEmpty then None
        else if segments.isEmpty && line.contains('#') then None
        else if segments.isEmpty then Some(1)
        else if line.head == '.' then waysToSolve(line.tail, segments)
        else
            cache2.get((line, segments)) match
                case Some(value) => value
                case None =>
                    val untilBroken = line.takeWhile(_ != '#')
                    val withBroken  = line.drop(untilBroken.length).takeWhile(_ != '.')

                    val totalLine = untilBroken ++ withBroken
                    // println(s"solving $totalLine segments ${segments.head}")

                    val result =
                        if cache.contains((totalLine, segments.head)) then
                            // println("CACHE HIT")
                            cache((totalLine, segments.head))
                        else
                            val r = dfs(
                              List((totalLine, 0, 0)),
                              segments.head,
                              ZSet.empty
                            ).toMap
                            cache.update((totalLine, segments.head), r)
                            r

                    // println(s"ways to solve is $result")

                    val a = result.map { case (lineSize, count) =>
                        val remainingLineWays = waysToSolve(line.drop(lineSize), segments.tail)
                        remainingLineWays.map(_ * count)
                    }

                    // println("solved a segment")

                    // println(s"got $a")
                    // println()

                    val b = a.filter(_.nonEmpty).flatten.toList

                    val r =
                        if b.isEmpty then None
                        else Some(b.sum)

                    cache2.update((line, segments), r)
                    r

    def part1(lines: List[String]): Long =
        lines
            .map { str =>
                val Array(line, segmentsStr) = str.split(" ")
                val segments                 = segmentsStr.split(",").map(_.toInt).toList
                (line, segments)
            }
            .map { case s => (s._1, waysToSolve(s._1, s._2)) }
            .map { case (s, ways) =>
                val answer = ways.getOrElse(0L)
                println(s"line $s has $answer ways")
                answer
            }
            .sum

    def part2(lines: List[String]): Long =
        lines
            .map { str =>
                val Array(line, segmentsStr) = str.split(" ")
                val segments                 = segmentsStr.split(",").map(_.toInt).toList
                (line, segments)
            }
            .par
            .map { case s =>
                val newLine = (1 to 5).map(_ => s._1).mkString("?")
                val answer  = (s._1, waysToSolve(newLine, s._2 ++ s._2 ++ s._2 ++ s._2 ++ s._2))

                println(s"line ${s._1} has $answer ways")

                answer
            }
            .map { case (s, ways) =>
                val answer = ways.getOrElse(0L)
                // println(s"line $s has $answer ways")
                answer
            }
            .sum

    def part2Old(lines: List[String]): Long =
        // return 0
        lines
            .map { str =>
                val Array(line, segmentsStr) = str.split(" ")
                val segments                 = segmentsStr.split(",").map(_.toInt).toList
                (line, segments)
            }
            .map { case (line, segments) =>
                println(s"SOLVING $line")
                val unionLine = s"$line?$line"
                val ans       = waysToSolve(unionLine, segments ++ segments)
                println(s"UNION LINE1 $ans")

                val unionLine2 = s"?$line?$line"
                val ans2       = waysToSolve(unionLine2, segments ++ segments)
                println(s"UNION LINE2 $ans2")

                val unionLine3 = s"?$line"
                val ans3       = waysToSolve(unionLine3, segments)
                println(s"UNION LINE3 $ans3")

                val unionLine4 = s"?$line?$line?"
                val ans4       = waysToSolve(unionLine4, segments ++ segments)
                println(s"UNION LINE4 $ans4")

                val unionLine5 = s"$line?$line?"
                val ans5       = waysToSolve(unionLine5, segments ++ segments)
                println(s"UNION LINE5 $ans5")

                val unionLine6 = s"$line?"
                val ans6       = waysToSolve(unionLine6, segments)
                println(s"UNION LINE6 $ans6")

                val vanilla = waysToSolve(line, segments).getOrElse(0L)

                val solutionMap = Map(
                  "line"        -> vanilla,
                  "?line"       -> waysToSolve(s"?$line", segments).get,
                  "line?"       -> waysToSolve(s"$line?", segments).get,
                  "?line?"      -> waysToSolve(s"?$line?", segments).get,
                  "?line?line"  -> waysToSolve(s"?$line?$line", segments ++ segments).get,
                  "?line?line?" -> waysToSolve(s"?$line?$line?", segments ++ segments).get,
                  "line?line?"  -> waysToSolve(s"$line?$line?", segments ++ segments).get,
                  "line?line"   -> waysToSolve(s"$line?$line", segments ++ segments).get,
                )
                val keys = solutionMap.keySet.toList

                val fullString = "line?line?line?line?line"

                def possibleCombinations(search: List[(List[String], String)], res: List[List[String]])
                    : List[List[String]] =
                    if (search.isEmpty) res
                    else
                        val (currentTaken, remainingList) = search.head
                        if (remainingList.isEmpty) possibleCombinations(search.tail, currentTaken.reverse +: res)
                        else
                            val newSearches = keys.flatMap { k =>
                                if remainingList.startsWith(k) then
                                    val newSearch = (k +: currentTaken, remainingList.drop(k.length))
                                    Some(newSearch)
                                else None
                            }
                            possibleCombinations(newSearches ++ search.tail, res)

                val p = possibleCombinations(List((List(), fullString)), List.empty)

                val exp2 = if (line.endsWith("#")) {
                    List(
                      solutionMap("line") * solutionMap("line") * solutionMap("line") * solutionMap(
                        "line"
                      ) * solutionMap("line"),
                      solutionMap("line?line?") * solutionMap("line?line?") * solutionMap("line"),
                      solutionMap("line?line?") * solutionMap("line") * solutionMap("line") * solutionMap("line")
                    ).max
                } else {
                    p.map { strings =>
                        strings.map(solutionMap).product
                    }.max
                }

                println(s"ALL POSSIBLE COMBINATIONS ${p}")

                val experimental = if (line.endsWith("#")) {
                    List(
                      vanilla * vanilla * vanilla * vanilla * vanilla,
                      ans.getOrElse(0L) * ans.getOrElse(0L) * vanilla,
                      ans.getOrElse(0L) * vanilla * vanilla
                    ).max
                } else {
                    List(
                      vanilla * vanilla * vanilla * vanilla * vanilla,
                      ans6.getOrElse(0L) * ans6.getOrElse(0L) * ans6.getOrElse(0L) * ans6.getOrElse(0L) * vanilla,
                      ans3.getOrElse(0L) * ans3.getOrElse(0L) * ans3.getOrElse(0L) * ans3.getOrElse(0L) * vanilla,

                      /*ans4.getOrElse(0L) * ans5.getOrElse(0L) * vanilla,
                      ans5.getOrElse(0L) * ans5.getOrElse(0L) * vanilla,
                      ans.getOrElse(0L) * ans4.getOrElse(0L) * vanilla,
                      ans6.getOrElse(0L) * ans6.getOrElse(0L) * ans6.getOrElse(0L) * ans6.getOrElse(0L) * vanilla,
                      ans3.getOrElse(0L) * ans3.getOrElse(0L) * ans3.getOrElse(0L) * ans3.getOrElse(0L) * vanilla,
                      vanilla * vanilla * vanilla * vanilla * vanilla,
                      ans.getOrElse(0L) * ans.getOrElse(0L) * vanilla*/
                    ).max
                }

                println(s"EXPERIMENTAL ANSWER ${experimental}")
                println(s"EXPERIMENTAL ANSWER2 ${exp2}")

                /*val qBefore = if (line.endsWith("#")) vanilla else waysToSolve(s"?$line", segments).getOrElse(0L)
                // val qBefore = waysToSolve(s"?$line", segments).getOrElse(0L)
                val qAfter = waysToSolve(s"$line?", segments).getOrElse(0L)

                val sorted = List(vanilla, qBefore, qAfter).sorted

                (line, sorted(2) * sorted(2) * sorted(2) * sorted(2) * sorted(1))*/
                (line, experimental)
                (line, exp2)
            }
            .map { case (s, ways) =>
                val answer = ways
                println(s"line $s has $answer ways")
                answer
            }
            .sum
