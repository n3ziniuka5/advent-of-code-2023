package aoc

import aoc.Common.timed

import scala.collection.parallel.CollectionConverters.*
import scala.io.Source
import collection.mutable.PriorityQueue
import scala.collection.mutable

object Day17:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day17.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class SearchStateKey(
        currentPos: Point,
        previousPos: Point,
        totalHeatLoss: Long,
    )
    case class SearchState(
        currentPos: Point,
        previousPos: Point,
        movesInSameDirection: Int,
        totalHeatLoss: Long,
        targetX: Int,
        targetY: Int
    ):
        def toKey = SearchStateKey(currentPos, previousPos, totalHeatLoss)

    object SearchState:
        given Ordering[SearchState] = Ordering.by { s =>
            val bestCaseDistance =
                Math.abs(s.currentPos.x - s.targetX) + Math.abs(s.currentPos.y - s.targetY) + s.totalHeatLoss
            (-bestCaseDistance, -s.totalHeatLoss)
        }

    def solve(
        searches: PriorityQueue[SearchState],
        map2d: Map2d[Int],
        visited: Map[SearchStateKey, (Int, Long)]
    ): Long =
        val head = searches.dequeue()
        // println(s"current state $head")

        def alreadySeen(search: SearchState) = visited.get(search.toKey).exists { case (existingSteps, existingHeat) =>
            existingSteps <= search.movesInSameDirection && existingHeat <= search.totalHeatLoss
        }

        if (head.currentPos.x == map2d.maxX && head.currentPos.y == map2d.maxY) head.totalHeatLoss
        else if (alreadySeen(head)) solve(searches, map2d, visited)
        else
            val newSearches = if (head.currentPos.up == head.previousPos)
                val moveDown =
                    if (head.movesInSameDirection < 3)
                        List(
                          head.copy(
                            head.currentPos.down,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.down).getOrElse(0)
                          )
                        )
                    else Nil

                val moveLeft = List(
                  head.copy(
                    head.currentPos.left,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.left).getOrElse(0)
                  )
                )

                val moveRight = List(
                  head.copy(
                    head.currentPos.right,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.right).getOrElse(0)
                  )
                )

                moveDown ++ moveLeft ++ moveRight
            else if (head.currentPos.down == head.previousPos)
                val moveUp =
                    if (head.movesInSameDirection < 3)
                        List(
                          head.copy(
                            head.currentPos.up,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.up).getOrElse(0)
                          )
                        )
                    else Nil

                val moveLeft = List(
                  head.copy(
                    head.currentPos.left,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.left).getOrElse(0)
                  )
                )

                val moveRight = List(
                  head.copy(
                    head.currentPos.right,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.right).getOrElse(0)
                  )
                )

                moveUp ++ moveLeft ++ moveRight
            else if (head.currentPos.left == head.previousPos)
                val moveRight =
                    if (head.movesInSameDirection < 3)
                        List(
                          head.copy(
                            head.currentPos.right,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.right).getOrElse(0)
                          )
                        )
                    else Nil

                val moveUp = List(
                  head.copy(
                    head.currentPos.up,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.up).getOrElse(0)
                  )
                )

                val moveDown = List(
                  head.copy(
                    head.currentPos.down,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.down).getOrElse(0)
                  )
                )

                moveUp ++ moveDown ++ moveRight
            else // head.currentPos.right == head.previousPos
                val moveLeft =
                    if (head.movesInSameDirection < 3)
                        List(
                          head.copy(
                            head.currentPos.left,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.left).getOrElse(0)
                          )
                        )
                    else Nil

                val moveUp = List(
                  head.copy(
                    head.currentPos.up,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.up).getOrElse(0)
                  )
                )

                val moveDown = List(
                  head.copy(
                    head.currentPos.down,
                    head.currentPos,
                    1,
                    head.totalHeatLoss + map2d.get(head.currentPos.down).getOrElse(0)
                  )
                )

                moveUp ++ moveDown ++ moveLeft

            newSearches.filter(_.currentPos.inBounds(map2d)).filterNot(alreadySeen).foreach(searches.enqueue(_))
            solve(searches, map2d, visited + (head.toKey -> (head.movesInSameDirection, head.totalHeatLoss)))

    def solve2(
        searches: PriorityQueue[SearchState],
        map2d: Map2d[Int],
        visited: Map[SearchStateKey, (Int, Long)]
    ): Long =
        val head = searches.dequeue()
        // println(s"current state $head")

        def alreadySeen(search: SearchState) = visited.get(search.toKey).exists { case (existingSteps, existingHeat) =>
            existingSteps <= search.movesInSameDirection && existingHeat <= search.totalHeatLoss
        }

        if (head.currentPos.x == map2d.maxX && head.currentPos.y == map2d.maxY) head.totalHeatLoss
        else if (alreadySeen(head)) solve2(searches, map2d, visited)
        else
            val newSearches = if (head.currentPos.up == head.previousPos)
                val moveDown =
                    if (head.movesInSameDirection < 10)
                        List(
                          head.copy(
                            head.currentPos.down,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.down).getOrElse(0)
                          )
                        )
                    else Nil

                val moveLeft = List(
                  head.copy(
                    head.currentPos.left.left.left.left,
                    head.currentPos.left.left.left,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.left,
                      head.currentPos.left.left,
                      head.currentPos.left.left.left,
                      head.currentPos.left.left.left.left
                    ).flatMap(map2d.get).sum
                  )
                )

                val moveRight = List(
                  head.copy(
                    head.currentPos.right.right.right.right,
                    head.currentPos.right.right.right,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.right,
                      head.currentPos.right.right,
                      head.currentPos.right.right.right,
                      head.currentPos.right.right.right.right
                    ).flatMap(map2d.get).sum
                  )
                )

                moveDown ++ moveLeft ++ moveRight
            else if (head.currentPos.down == head.previousPos)
                val moveUp =
                    if (head.movesInSameDirection < 10)
                        List(
                          head.copy(
                            head.currentPos.up,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.up).getOrElse(0)
                          )
                        )
                    else Nil

                val moveLeft = List(
                  head.copy(
                    head.currentPos.left.left.left.left,
                    head.currentPos.left.left.left,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.left,
                      head.currentPos.left.left,
                      head.currentPos.left.left.left,
                      head.currentPos.left.left.left.left
                    ).flatMap(map2d.get).sum
                  )
                )

                val moveRight = List(
                  head.copy(
                    head.currentPos.right.right.right.right,
                    head.currentPos.right.right.right,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.right,
                      head.currentPos.right.right,
                      head.currentPos.right.right.right,
                      head.currentPos.right.right.right.right
                    ).flatMap(map2d.get).sum
                  )
                )

                moveUp ++ moveLeft ++ moveRight
            else if (head.currentPos.left == head.previousPos)
                val moveRight =
                    if (head.movesInSameDirection == 0)
                        List(
                          head.copy(
                            head.currentPos.right.right.right.right,
                            head.currentPos.right.right.right,
                            4,
                            head.totalHeatLoss + List(
                              head.currentPos.right,
                              head.currentPos.right.right,
                              head.currentPos.right.right.right,
                              head.currentPos.right.right.right.right
                            ).flatMap(map2d.get).sum
                          )
                        )
                    else if (head.movesInSameDirection < 10)
                        List(
                          head.copy(
                            head.currentPos.right,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.right).getOrElse(0)
                          )
                        )
                    else Nil

                val moveUp = List(
                  head.copy(
                    head.currentPos.up.up.up.up,
                    head.currentPos.up.up.up,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.up,
                      head.currentPos.up.up,
                      head.currentPos.up.up.up,
                      head.currentPos.up.up.up.up
                    ).flatMap(map2d.get).sum
                  )
                )

                val moveDown = List(
                  head.copy(
                    head.currentPos.down.down.down.down,
                    head.currentPos.down.down.down,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.down,
                      head.currentPos.down.down,
                      head.currentPos.down.down.down,
                      head.currentPos.down.down.down.down
                    ).flatMap(map2d.get).sum
                  )
                )

                moveUp ++ moveDown ++ moveRight
            else // head.currentPos.right == head.previousPos
                val moveLeft =
                    if (head.movesInSameDirection < 10)
                        List(
                          head.copy(
                            head.currentPos.left,
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map2d.get(head.currentPos.left).getOrElse(0)
                          )
                        )
                    else Nil

                val moveUp = List(
                  head.copy(
                    head.currentPos.up.up.up.up,
                    head.currentPos.up.up.up,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.up,
                      head.currentPos.up.up,
                      head.currentPos.up.up.up,
                      head.currentPos.up.up.up.up
                    ).flatMap(map2d.get).sum
                  )
                )

                val moveDown = List(
                  head.copy(
                    head.currentPos.down.down.down.down,
                    head.currentPos.down.down.down,
                    4,
                    head.totalHeatLoss + List(
                      head.currentPos.down,
                      head.currentPos.down.down,
                      head.currentPos.down.down.down,
                      head.currentPos.down.down.down.down
                    ).flatMap(map2d.get).sum
                  )
                )

                moveUp ++ moveDown ++ moveLeft

            newSearches.filter(_.currentPos.inBounds(map2d)).filterNot(alreadySeen).foreach(searches.enqueue(_))
            solve2(searches, map2d, visited + (head.toKey -> (head.movesInSameDirection, head.totalHeatLoss)))

    def part1(lines: List[String]): Long =
        0

    def part2(lines: List[String]): Long =
        val map = Map2d.fromLines(lines).map { case (k, v) =>
            k -> v.toString.toInt
        }

        val startingPoint = Point(0, 0)
        val pq = mutable.PriorityQueue[SearchState](
          SearchState(startingPoint, startingPoint.left, 0, 0, map.maxX, map.maxY)
        )

        solve2(pq, map, Map.empty)
