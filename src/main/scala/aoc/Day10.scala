package aoc

import aoc.Common.timed

import scala.io.Source

object Day10:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day10.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Pos(x: Int, y: Int):
        def neighbors: List[Pos] = List(
          Pos(x - 1, y),
          Pos(x + 1, y),
          Pos(x, y - 1),
          Pos(x, y + 1),
        )

    def parse(lines: List[String]): Map[Pos, Char] =
        lines.zipWithIndex.flatMap { (line, y) =>
            line.zipWithIndex.map { (c, x) =>
                Pos(x, y) -> c
            }
        }.toMap

    def findLoop(map: Map[Pos, Char], startingPoint: Pos): List[Pos] =
        def loop(currentPos: Pos, map: Map[Pos, Char], path: List[Pos], visited: Set[Pos]): List[Pos] =
            if (currentPos == startingPoint && path.nonEmpty) path
            else
                val currentDirection = map(currentPos)
                val nextPos: List[Pos] = currentDirection match
                    case '|' =>
                        val up   = currentPos.copy(y = currentPos.y - 1)
                        val down = currentPos.copy(y = currentPos.y + 1)
                        val canGoUp =
                            if (map.get(up).contains('|') || map.get(up).contains('F') || map.get(up).contains('7'))
                                true
                            else false
                        val canGoDown =
                            if (
                              map.get(down).contains('|') || map.get(down).contains('J') || map.get(down).contains('L')
                            ) true
                            else false

                        Option.when(canGoUp)(up).toList ++ Option.when(canGoDown)(down).toList
                    case '-' =>
                        val left  = currentPos.copy(x = currentPos.x - 1)
                        val right = currentPos.copy(x = currentPos.x + 1)

                        val canGoLeft =
                            if (
                              map.get(left).contains('-') || map.get(left).contains('L') || map.get(left).contains('F')
                            ) true
                            else false
                        val canGoRight =
                            if (
                              map.get(right).contains('-') || map.get(right).contains('J') || map
                                  .get(right)
                                  .contains('7')
                            ) true
                            else false

                        Option.when(canGoLeft)(left).toList ++ Option.when(canGoRight)(right).toList

                    case 'L' =>
                        currentPos.copy(x = currentPos.x + 1, y = currentPos.y + 1)
                        val up    = currentPos.copy(y = currentPos.y - 1)
                        val right = currentPos.copy(x = currentPos.x + 1)

                        val canGoUp =
                            if (map.get(up).contains('|') || map.get(up).contains('F') || map.get(up).contains('7'))
                                true
                            else false
                        val canGoRight =
                            if (
                              map.get(right).contains('-') || map.get(right).contains('J') || map
                                  .get(right)
                                  .contains('7')
                            ) true
                            else false

                        Option.when(canGoUp)(up).toList ++ Option.when(canGoRight)(right).toList

                    case 'J' =>
                        currentPos.copy(x = currentPos.x - 1, y = currentPos.y + 1)
                        val up   = currentPos.copy(y = currentPos.y - 1)
                        val left = currentPos.copy(x = currentPos.x - 1)

                        val canGoUp =
                            if (map.get(up).contains('|') || map.get(up).contains('F') || map.get(up).contains('7'))
                                true
                            else false
                        val canGoLeft =
                            if (
                              map.get(left).contains('-') || map.get(left).contains('L') || map.get(left).contains('F')
                            ) true
                            else false

                        Option.when(canGoUp)(up).toList ++ Option.when(canGoLeft)(left).toList

                    case '7' =>
                        currentPos.copy(x = currentPos.x - 1, y = currentPos.y - 1)
                        val down = currentPos.copy(y = currentPos.y + 1)
                        val left = currentPos.copy(x = currentPos.x - 1)

                        val canGoDown =
                            if (
                              map.get(down).contains('|') || map.get(down).contains('J') || map.get(down).contains('L')
                            ) true
                            else false

                        val canGoLeft =
                            if (
                              map.get(left).contains('-') || map.get(left).contains('L') || map.get(left).contains('F')
                            ) true
                            else false

                        Option.when(canGoDown)(down).toList ++ Option.when(canGoLeft)(left).toList

                    case 'F' =>
                        currentPos.copy(x = currentPos.x + 1, y = currentPos.y - 1)
                        val down  = currentPos.copy(y = currentPos.y + 1)
                        val right = currentPos.copy(x = currentPos.x + 1)

                        val canGoDown =
                            if (
                              map.get(down).contains('|') || map.get(down).contains('J') || map.get(down).contains('L')
                            ) true
                            else false

                        val canGoRight =
                            if (
                              map.get(right).contains('-') || map.get(right).contains('J') || map
                                  .get(right)
                                  .contains('7')
                            ) true
                            else false

                        Option.when(canGoDown)(down).toList ++ Option.when(canGoRight)(right).toList

                if (nextPos.sizeIs == 2) {
                    nextPos.filterNot(visited.contains).headOption match
                        case Some(pos) =>
                            println(s"moving to $pos")
                            val newMap = map + (currentPos -> ' ')
                            loop(pos, map, currentPos +: path, visited + currentPos)
                        case None =>
                            if (nextPos.contains(startingPoint)) {
                                loop(startingPoint, map, currentPos +: path, visited + currentPos)
                            } else {
                                Nil
                            }
                } else {
                    Nil
                }

        LazyList('|', '-', 'L', 'J', '7', 'F')
            .map { startDirection =>
                val newMap = map + (startingPoint -> startDirection)
                loop(startingPoint, newMap, List.empty, Set.empty)
            }
            .find(_.nonEmpty)
            .get

    def part1(lines: List[String]): Long =
        val map           = parse(lines)
        val startingPoint = map.find(_._2 == 'S').get._1
        val loop          = findLoop(map, startingPoint)
        // println(s"found loop $loop")
        (loop.size + 1) / 2

    enum Rotation:
        case Clockwise, CounterClockwise

    def part2(lines: List[String]): Long =
        val map           = parse(lines)
        val startingPoint = map.find(_._2 == 'S').get._1
        val loop          = findLoop(map, startingPoint)
        // val loop = findLoop(map, startingPoint).reverse.tail :+ startingPoint

        println(s"GOING TO LOOP ${loop(0)} ${loop(1)}")

        val loopSet = loop.toSet

        def loopForever: LazyList[Pos] = LazyList.from(loop) lazyAppendedAll loopForever

        println("-------------")
        println(s"${loopForever(0)} ${map(loopForever(0))}")
        println("-------------")

        def findRotation(positions: LazyList[Pos], previous: Pos, left: Int, right: Int): Rotation =
            val current = positions.head
            if current == startingPoint && left + right > 0 then
                println(s"found $left left turns and $right right turns")
                if left > right then Rotation.CounterClockwise else Rotation.Clockwise
            else
                val next = positions.tail.head

                val turnLeft1 = previous.x < current.x && previous.y == current.y && map(next) == 'J'
                val turnLeft2 = previous.x == current.x && previous.y < current.y && map(next) == 'L'
                val turnLeft3 = previous.x > current.x && previous.y == current.y && map(next) == 'F'
                val turnLeft4 = previous.x == current.x && previous.y > current.y && map(next) == '7'

                val turnLeft = turnLeft1 || turnLeft2 || turnLeft3 || turnLeft4

                if (turnLeft) {
                    findRotation(
                      positions.tail,
                      current,
                      left + 1,
                      right
                    )
                } else {
                    val l         = List(previous, current, next)
                    val straight1 = l.map(_.x).distinct.size == 1 && l.map(_.y).distinct.size == 3
                    val straight2 = l.map(_.x).distinct.size == 3 && l.map(_.y).distinct.size == 1

                    if (straight1 || straight2) {
                        findRotation(
                          positions.tail,
                          current,
                          left,
                          right
                        )
                    } else {
                        findRotation(
                          positions.tail,
                          current,
                          left,
                          right + 1
                        )
                    }
                }

        def searchUntilWallHit(from: List[Pos], acc: Set[Pos]): Set[Pos] =
            if (from.isEmpty) acc
            else
                val current = from.head
                // println(s"searching $current ${map(current)}")
                map(current) // thorws exception if we're going wrong direction
                if (loopSet.contains(current)) searchUntilWallHit(from.tail, acc)
                else
                    val next = current.neighbors.filterNot(acc.contains)
                    searchUntilWallHit(next ++ from.tail, acc + current)

        def findEnclosed(positions: LazyList[Pos], previous: Pos, rotation: Rotation, result: Set[Pos]): Set[Pos] =
            val current = positions.head
            if current == startingPoint then
                val next = positions.tail.head

                if (rotation == Rotation.Clockwise) {
                    val lookTopLeft =
                        previous.x == current.x && previous.y < current.y && next.x < current.x && next.y == current.y
                    val lookTopRight =
                        previous.x > current.x && previous.y == current.y && next.x == current.x && next.y < current.y
                    val lookBottomRight =
                        previous.x == current.x && previous.y > current.y && next.x > current.x && next.y == current.y
                    val lookBottomLeft =
                        previous.x < current.x && previous.y == current.y && next.x == current.x && next.y > current.y

                    val newPoints = if (lookTopLeft) {
                        println(s"need to look top left at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x - 1, y = current.y - 1)), Set.empty)
                    } else if (lookTopRight) {
                        println(s"need to look top right at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x + 1, y = current.y - 1)), Set.empty)
                    } else if (lookBottomRight) {
                        println(s"need to look bottom right at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x + 1, y = current.y + 1)), Set.empty)
                    } else if (lookBottomLeft) {
                        println(s"need to look bottom left at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x - 1, y = current.y + 1)), Set.empty)
                    } else {
                        Set.empty
                    }

                    // println(s"search at $current complete, RETURNING RESULT")
                    result ++ newPoints
                } else {
                    ??? // both test and my input are clockwise so I don't care
                }
            else
                val next = positions.tail.head

                if (rotation == Rotation.Clockwise) {
                    val lookTopLeft =
                        previous.x == current.x && previous.y < current.y && next.x < current.x && next.y == current.y
                    val lookTopRight =
                        previous.x > current.x && previous.y == current.y && next.x == current.x && next.y < current.y
                    val lookBottomRight =
                        previous.x == current.x && previous.y > current.y && next.x > current.x && next.y == current.y
                    val lookBottomLeft =
                        previous.x < current.x && previous.y == current.y && next.x == current.x && next.y > current.y

                    val truths = List(lookTopLeft, lookTopRight, lookBottomRight, lookBottomLeft).filter(identity)
                    if truths.size > 1 then
                        println("FOUND ERROR")
                        System.exit(1)

                    val newPoints = if (lookTopLeft) {
                        // println(s"need to look top left at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x - 1, y = current.y - 1)), Set.empty)
                    } else if (lookTopRight) {
                        // println(s"need to look top right at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x + 1, y = current.y - 1)), Set.empty)
                    } else if (lookBottomRight) {
                        // println(s"need to look bottom right at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x + 1, y = current.y + 1)), Set.empty)
                    } else if (lookBottomLeft) {
                        // println(s"need to look bottom left at $current ${map(current)}")
                        searchUntilWallHit(List(current.copy(x = current.x - 1, y = current.y + 1)), Set.empty)
                    } else {
                        Set.empty
                    }

                    // println(s"search at $current complete")
                    findEnclosed(positions.tail, current, rotation, result ++ newPoints)
                } else {
                    ??? // both test and my input are clockwise so I don't care
                }

        // findEnclosed(loopForever, loop.last)
        println("loop found")
        println
        val rotation = findRotation(loopForever, loop.last, 0, 0)
        println(s"rotation is $rotation")
        println

        val enclosedPoints = findEnclosed(loopForever, loop.last, rotation, Set.empty)

        val maxX = map.keys.maxBy(_.x).x
        val maxY = map.keys.maxBy(_.y).y

        val pathVisual = (for {
            y <- 0 to maxY
            x <- 0 to maxX
        } yield {
            val pos = Pos(x, y)
            if (loopSet.contains(pos)) map(pos)
            else '.'
        }).grouped(maxX + 1).map(_.mkString).mkString("\n").map {
            case 'F' => '╔'
            case '7' => '╗'
            case 'J' => '╝'
            case 'L' => '╚'
            case '|' => '║'
            case '-' => '═'
            case c   => c
        }

        println(pathVisual)

        val enclosedVisual = (for {
            y <- 0 to maxY
            x <- 0 to maxX
        } yield {
            val pos = Pos(x, y)
            if (enclosedPoints.contains(pos)) '*'
            else if (loopSet.contains(pos)) map(pos)
            else '.'
        }).grouped(maxX + 1).map(_.mkString).mkString("\n").map {
            case 'F' => '╔'
            case '7' => '╗'
            case 'J' => '╝'
            case 'L' => '╚'
            case '|' => '║'
            case '-' => '═'
            case c   => c
        }

        println(enclosedVisual)

        enclosedPoints.size
        // enclosedPoints.count(map(_) == '.')
