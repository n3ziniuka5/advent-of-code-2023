package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source
import scala.collection.parallel.CollectionConverters.*

object Day21:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day21.txt").getLines().toList
        timed("Part 1", part1(lines, 64))
        // timed("Part 2", part2(lines, 26501365))
        timed("Part 2", part2(lines, 500))

    var globalMaxSteps = 0L
    def pathScore(p: Map[Point, Long]): Long =
        p.count { case (_, s) =>
            val remainingSteps = globalMaxSteps - s
            remainingSteps >= 0 && remainingSteps % 2 == globalMaxSteps % 2
        }

    def pathScore2(p: Map[Point, (Point, Long)]): Long =
        p.count { case (_, (_, s)) =>
            val remainingSteps = globalMaxSteps - s
            remainingSteps >= 0 && remainingSteps % 2 == globalMaxSteps % 2
        }

    def part1(lines: List[String], maxSteps: Int): Long =
        val map           = Map2DVec.fromLines(lines)
        val startingPoint = Point(map.maxX / 2, map.maxY / 2)

        val pq = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))
        shortestPaths(pq, map, Map.empty).count { case (_, s) =>
            val remainingSteps = maxSteps - s
            remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
        }

    var timeSpent             = 0L
    var spentUsingCache: Long = 0L

    def part2(lines: List[String], maxSteps: Int): Long =
        globalMaxSteps = maxSteps
        val initialMap    = Map2DVec.fromLines(lines)
        val startingPoint = Point(initialMap.maxX / 2, initialMap.maxY / 2)
        val pq            = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))

        val centerPaths = shortestPaths(pq, initialMap, Map.empty)

        @tailrec
        def mapSearches(searches: Queue[Point], maps: Map[Point, Map[Point, Long]], score: Long): Long =
            if (searches.isEmpty)
                // println(s"processed ${maps.size} maps")
                score
            else
                val (head, rest) = searches.dequeue
                if (maps.contains(head)) mapSearches(rest, maps, score)
                else if (head == Point(0, 0))
                    val newMaps     = maps + (head -> centerPaths)
                    val newSearches = head.adjacent
                    mapSearches(rest.enqueueAll(newSearches), newMaps, pathScore(centerPaths))
                else
                    val hasMapAbove      = maps.contains(Point(head.x, head.y - 1))
                    val hasMapBelow      = maps.contains(Point(head.x, head.y + 1))
                    val hasMapToTheLeft  = maps.contains(Point(head.x - 1, head.y))
                    val hasMapToTheRight = maps.contains(Point(head.x + 1, head.y))

                    /*println(
                      s"searching from $head hasMapAbove $hasMapAbove hasMapBelow $hasMapBelow hasMapToTheLeft $hasMapToTheLeft hasMapToTheRight $hasMapToTheRight"
                    )*/

                    // println(s"currentScore $score")

                    val start = System.currentTimeMillis()
                    val newPaths = if (hasMapAbove) {
                        val mapAbove = maps(Point(head.x, head.y - 1))
                        val searches = (0 to initialMap.maxX).toList.map { x =>
                            val p             = Point(x, 0)
                            val opposingPoint = Point(x, initialMap.maxY)

                            (p, mapAbove(opposingPoint) + 1)
                        }
                        // println()

                        val res = shortestPathsCached(searches, initialMap)

                        // val aboveScore = pathScore(mapAbove)
                        // val thisScore  = pathScore2(res)

                        /*println(
                          s"pathAbove - $aboveScore thisScore - $thisScore maxStepAbove ${mapAbove.maxBy(_._2)._2} maxStepThis ${res.maxBy(_._2)._2}"
                        )*/

                        /*if(aboveScore == 42) {
                            mapAbove.map((k, v) => (k, v + ))
                        } else {
                            res
                        }*/
                        res.view.mapValues(_._2).toMap // TODO optimize
                    } else if (hasMapBelow) {
                        val mapBelow = maps(Point(head.x, head.y + 1))
                        val searches = (0 to initialMap.maxX).toList.map { x =>
                            val p             = Point(x, initialMap.maxY)
                            val opposingPoint = Point(x, 0)
                            (p, mapBelow(opposingPoint) + 1)
                        }
                        /*val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
                        shortestPaths(pq, initialMap, Map.empty)*/

                        val res = shortestPathsCached(searches, initialMap)
                        res.view.mapValues(_._2).toMap // TODO optimize
                    } else if (hasMapToTheLeft) {
                        val mapToTheLeft = maps(Point(head.x - 1, head.y))
                        val searches = (0 to initialMap.maxY).toList.map { y =>
                            val p             = Point(0, y)
                            val opposingPoint = Point(initialMap.maxX, y)
                            (p, mapToTheLeft(opposingPoint) + 1)
                        }
                        /*val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
                        shortestPaths(pq, initialMap, Map.empty)*/
                        val res = shortestPathsCached(searches, initialMap)
                        res.view.mapValues(_._2).toMap // TODO optimize
                    } else if (hasMapToTheRight) {
                        val mapToTheRight = maps(Point(head.x + 1, head.y))
                        val searches = (0 to initialMap.maxY).toList.map { y =>
                            val p             = Point(initialMap.maxX, y)
                            val opposingPoint = Point(0, y)
                            (p, mapToTheRight(opposingPoint) + 1)
                        }
                        /*val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
                        shortestPaths(pq, initialMap, Map.empty)*/
                        val res = shortestPathsCached(searches, initialMap)
                        res.view.mapValues(_._2).toMap // TODO optimize
                    } else {
                        ???
                    }

                    val end = System.currentTimeMillis()
                    timeSpent = timeSpent + (end - start)

                    val addedScore = pathScore(newPaths)

                    // println(s"$end $start diff - ${end - start}")

                    if (addedScore > 0) {
                        val newMaps     = maps + (head -> newPaths)
                        val newSearches = head.adjacent
                        mapSearches(rest.enqueueAll(newSearches), newMaps, score + addedScore)
                    } else {
                        mapSearches(rest, maps, score)
                    }

        timeSpent = 0L
        spentUsingCache = 0L

        cache.clear()
        cache2.clear()
        val r = mapSearches(Queue(Point(0, 0)), Map.empty, 0)

        println(s"timespent in main loop ${timeSpent}ms")
        println(s"timespent using cache ${spentUsingCache}ms")

        r
    /*val squareToTheLeftPaths = {
            val searches = (0 to initialMap.maxY).map { y =>
                val p             = Point(initialMap.maxX, y)
                val opposingPoint = Point(0, y)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        val squareToTheRightPaths = {
            val searches = (0 to initialMap.maxY).map { y =>
                val p             = Point(0, y)
                val opposingPoint = Point(initialMap.maxX, y)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        val squareToUpPaths = {
            val searches = (0 to initialMap.maxX).map { x =>
                val p             = Point(x, initialMap.maxY)
                val opposingPoint = Point(x, 0)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        val squareToDownPaths = {
            val searches = (0 to initialMap.maxX).map { x =>
                val p             = Point(x, 0)
                val opposingPoint = Point(x, initialMap.maxY)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        println(s"centerScore ${pathScore(centerPaths)}")
        println(s"squareToTheLeftScore ${pathScore(squareToTheLeftPaths)}")
        println(s"squareToTheRightScore ${pathScore(squareToTheRightPaths)}")
        println(s"squareUpScore ${pathScore(squareToUpPaths)}")
        println(s"squareDownScore ${pathScore(squareToDownPaths)}")

        val squareToTheLeftPaths2 = {
            val searches = (0 to initialMap.maxY).map { y =>
                val p             = Point(initialMap.maxX, y)
                val opposingPoint = Point(0, y)
                (p, squareToTheLeftPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }
        println(s"squareToTheLeftScore2 ${pathScore(squareToTheLeftPaths2)}")*/

    /*val initialMap = Map2DVec.fromLines(lines)

        val results = List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19 /*, 21, 23, 25, 27, 29, 31, 33, 36, 39, 41 */ ).map {
            repeats =>
                val expandedMap = Map2DVec.fromLines {
                    val increasedWidth = lines.map(l => l.repeat(repeats))
                    (1 to repeats).flatMap(_ => increasedWidth).toList
                }
                val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

                // println("MAP CONSTRUCTION COMPLETE")

                val pq    = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))
                val paths = shortestPaths(pq, expandedMap, Map.empty, maxSteps)
                val ans = paths.count { case (_, s) =>
                    val remainingSteps = maxSteps - s
                    remainingSteps % 2 == maxSteps % 2
                }

                val maxStepsInSearch = paths.maxBy(_._2)._2

                (repeats, ans, maxStepsInSearch, paths)

                // println(s"repeats - $repeats ans - $ans")
        }

        val ansGap: Long      = results(1)._2 - results(0)._2
        val maxStepsGap: Long = results(1)._3 - results(0)._3

        val cornerPoints = List(
          Point(0, 0),
          Point(initialMap.maxX, 0),
          Point(0, initialMap.maxY),
          Point(initialMap.maxX, initialMap.maxY)
        )

        val aaa = cornerPoints.map { p =>
            val s              = results(0)._4(p)
            val remainingSteps = maxSteps - s
            if (remainingSteps % 2 == maxSteps % 2) {
                println(s"CORNER GOOD ${s} ${maxSteps - s} ${(maxSteps - s) / 2}")
            }

            maxSteps - s
        }.sum
        println(s"corner sum $aaa")

        results.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - results(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - results(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        def loop(a: Long, s: Long, rotation: Long): Long =
            val newS = s + maxStepsGap
            if (newS > maxSteps)
                val remainingSteps = maxSteps - s

                val expandedMap = Map2DVec.fromLines {
                    val repeats        = 5
                    val increasedWidth = lines.map(l => l.repeat(repeats))
                    (1 to repeats).flatMap(_ => increasedWidth).toList
                }
                val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)
                val pq            = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))

                val remainingRes = shortestPaths(pq, expandedMap, Map.empty, remainingSteps).count { case (_, s) =>
                    val remainingSteps2 = remainingSteps - s
                    remainingSteps2 % 2 == remainingSteps % 2
                }

                println(
                  s"returning $a steps taken so far $s remaining steps $remainingSteps remaining res $remainingRes; x ${initialMap.maxX} y ${initialMap.maxY} rotations $rotation"
                )
                a
            else
                // println(s"currently $a, adding ${ansGap * rotation}")
                loop(a + ansGap * rotation, newS, rotation + 1)

        return loop(results(0)._2, results(0)._3, 1)

        println(s"ansGap $ansGap maxStepsGap $maxStepsGap")

        val expandedMap = Map2DVec.fromLines {
            val repeats        = 3
            val increasedWidth = lines.map(l => l.repeat(repeats))
            (1 to repeats).flatMap(_ => increasedWidth).toList
        }
        val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

        // println("MAP CONSTRUCTION COMPLETE")

        val pq = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))
        shortestPaths(pq, expandedMap, Map.empty, maxSteps).count { case (_, s) =>
            val remainingSteps = maxSteps - s
            remainingSteps % 2 == maxSteps % 2
        }*/

// println(initialMap)
// println()
// println(trippledMap)

    /*val startingPoint = (for {
            x <- 0 until initialMap.maxX
            y <- 0 until initialMap.maxY
        } yield Point(x, y)).find(p => initialMap(p) == 'S').get*/

    /*val allShortestPaths = (for {
            x <- 0 until initialMap.maxX
            y <- 0 until initialMap.maxY
        } yield Point(x, y)).par.map { p =>
            p -> shortestPaths(Queue((p, 0)), initialMap, Map.empty)
        }.toMap*/

// val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

    /*shortestPaths(Queue((startingPoint, 0)), expandedMap, Map.empty, 2).count { case (_, s) =>
            s <= maxSteps
        }*/

    val cache  = collection.mutable.Map.empty[String, Map[Point, (Point, Long)]]
    val cache2 = collection.mutable.Map.empty[String, Long]
    def shortestPathsCached(
        from: List[(Point, Long)],
        map: Map2DVec[Char]
    ): Map[Point, (Point, Long)] =

        /*val cacheKey =
            if (from.sizeIs == 1) {
                "none"
            } else if (from.head._1 == Point(0, 0) && from(1)._1 == Point(1, 0) && from.head._2 == from(1)._2 + 1) {
                "xIncDec"
            } else if (from.head._1 == Point(0, 0) && from(1)._1 == Point(1, 0) && from.head._2 == from(1)._2 - 1) {
                "xIncInc"
            } else {
                println(s"NO CACHE KEY $from")
                ???
            }*/
        val cacheKey =
            if (from.sizeIs == 1) {
                "none"
            } else {
                val minSteps = from.minBy(_._2)._2
                from.map(a => (a._1, a._2 - minSteps)).toString()
            }

        @tailrec
        def loop(
            search: mutable.PriorityQueue[(Point, Point, Long)],
            map: Map2DVec[Char],
            results: Map[Point, (Point, Long)]
        ): Map[Point, (Point, Long)] =
            if (search.isEmpty) results
            else
                val (headPoint, origin, headSteps) = search.dequeue()
                // println(s"running search from $headPoint")
                if (results.contains(headPoint)) loop(search, map, results)
                else
                    val newResults = results + (headPoint -> (origin, headSteps))
                    val newSearches = headPoint.adjacent
                        .filter(_.inBounds(map))
                        .filter(p => map(p) != '#')
                        .map(p => (p, origin, headSteps + 1))
                    search.enqueue(newSearches: _*)
                    loop(search, map, newResults)

        if (from.sizeIs == 1) {
            val pq = mutable.PriorityQueue(from.map(a => (a._1, a._1, a._2)): _*)(Ordering.by(_._3 * -1))
            loop(pq, map, Map.empty)
        } else {

            cache.get(cacheKey) match
                case None =>
                    val pq     = mutable.PriorityQueue(from.map(a => (a._1, a._1, a._2)): _*)(Ordering.by(_._3 * -1))
                    val result = loop(pq, map, Map.empty)

                    val deltas = result.map { case (point, (origin, steps)) =>
                        val delta = steps - result(origin)._2
                        (point, (origin, delta))
                    }

                    /*println(from)
                    println
                    println(result)
                    println
                    println(deltas)
                    ???*/

                    if (cacheKey != "none") {
                        cache.put(cacheKey, deltas)
                        cache2.put(cacheKey, pathScore2(result))

                        // cache2.put(cacheKey, )
                        /*println(s"puting $cacheKey $from")
                        println
                        println(s"added to cache $deltas")
                        println
                        println(s"result before cache $result")
                        println
                        println("---------------------------------------")*/
                    }

                    result
                case Some(cached) =>
                    val fromMap = from.toMap
                    val start   = System.currentTimeMillis()
                    val deltas = cached.map { case (point, (origin, delta)) =>
                        (point, (origin, delta + fromMap(origin)))
                    }
                    val end = System.currentTimeMillis()
                    spentUsingCache = spentUsingCache + (end - start)

                    val cachedScore = cache2(cacheKey)
                    val newScore    = pathScore2(deltas)

                    if (deltas.maxBy(_._2._2)._2._2 + 100 < globalMaxSteps) {
                        println(s"cachedScore $cachedScore newScore $newScore")
                    }

//                    val pq     = mutable.PriorityQueue(from.map(a => (a._1, a._1, a._2)): _*)(Ordering.by(_._3 * -1))
//                    val result = loop(pq, map, Map.empty)

                    /*println(s"using $cacheKey $from")
                    println
                    println(s"cached $cached")
                    println
                    println(s"cached result $deltas")
                    println
                    println(s"proper result $result")
                    ???*/

                    deltas

//            println("COULD CACHE HERE")
//            println(from)
//            println

        }

    @tailrec
    def shortestPaths(
        search: mutable.PriorityQueue[(Point, Long)],
        map: Map2DVec[Char],
        results: Map[Point, Long],
    ): Map[Point, Long] =
        if (search.isEmpty) results
        else
            val (headPoint, headSteps) = search.dequeue()
            // println(s"running search from $headPoint")
            if (results.contains(headPoint)) shortestPaths(search, map, results)
            else
                val newResults = results + (headPoint -> headSteps)
                val newSearches = headPoint.adjacent
                    .filter(_.inBounds(map))
                    .filter(p => map(p) != '#')
                    .map(p => (p, headSteps + 1))
                search.enqueue(newSearches: _*)
                shortestPaths(search, map, newResults)

//    @tailrec
//    def shortestPaths(
//        search: mutable.PriorityQueue[(Point, Long)],
//        map: Map2DVec[Char],
//        results: Map[Point, Long],
//        maxSteps: Long
//    ): Map[Point, Long] =
//        if (search.isEmpty) results
//        else
//            val (headPoint, headSteps) = search.dequeue()
//            // println(s"running search from $headPoint")
//            if (results.contains(headPoint)) shortestPaths(search, map, results, maxSteps)
//            else if (headSteps > maxSteps) shortestPaths(search, map, results, maxSteps)
//            else
//                val newResults = results + (headPoint -> headSteps)
//                val newSearches = headPoint.adjacent
//                    .filter(_.inBounds(map))
//                    .filter(p => map(p) != '#')
//                    .map(p => (p, headSteps + 1))
//                search.enqueue(newSearches: _*)
//                shortestPaths(search, map, newResults, maxSteps)

/*enum EO:
        case Even, Odd

        def flip: EO = this match
            case Even => Odd
            case Odd  => Even

    @tailrec
    def shortestPaths(
        search: mutable.PriorityQueue[(Point, EO, Int)],
        map: Map2DVec[Char],
        results: Map[Point, List[(EO, Int)]],
        maxSteps: Int
    ): Map[Point, List[(EO, Int)]] =
        if (search.isEmpty) results
        else
            val (headPoint, headEO, headSteps) = search.dequeue()
            // println(s"running search from $headPoint")
            if (results.get(headPoint).exists(_.exists(_._1 == headEO))) shortestPaths(search, map, results, maxSteps)
            else if (headSteps > maxSteps) shortestPaths(search, map, results, maxSteps)
            else
                val existing   = results.getOrElse(headPoint, List.empty)
                val newResults = results + (headPoint -> ((headEO, headSteps) +: existing))
                val newSearches = headPoint.adjacent
                    .filter(_.inBounds(map))
                    .filter(p => map(p) != '#')
                    .map(p => (p, headEO.flip, headSteps + 1))
                search.enqueue(newSearches: _*)
                shortestPaths(search, map, newResults, maxSteps)*/
