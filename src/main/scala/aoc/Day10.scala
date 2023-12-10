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

    def findLoop(map: Map[Pos, Char], startingPoint: Pos): (Map[Pos, Char], List[Pos]) =
        def loop(
            currentPos: Pos,
            map: Map[Pos, Char],
            path: List[Pos],
            visited: Set[Pos]
        ): (Map[Pos, Char], List[Pos]) =
            if (currentPos == startingPoint && path.nonEmpty) (map, path)
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
                            val newMap = map + (currentPos -> ' ')
                            loop(pos, map, currentPos +: path, visited + currentPos)
                        case None =>
                            if (nextPos.contains(startingPoint)) {
                                loop(startingPoint, map, currentPos +: path, visited + currentPos)
                            } else {
                                (Map.empty, Nil)
                            }
                } else {
                    (Map.empty, Nil)
                }

        LazyList('|', '-', 'L', 'J', '7', 'F')
            .map { startDirection =>
                val newMap = map + (startingPoint -> startDirection)
                loop(startingPoint, newMap, List.empty, Set.empty)
            }
            .find(_._2.nonEmpty)
            .get

    def part1(lines: List[String]): Long =
        val map           = parse(lines)
        val startingPoint = map.find(_._2 == 'S').get._1
        val loop          = findLoop(map, startingPoint)
        (loop._2.size + 1) / 2

    enum OutsideIsToThe:
        case Left, Right

    enum MovingTowards:
        case Up, Down, Left, Right

    def part2(lines: List[String]): Long =
        val map            = parse(lines)
        val startingPoint  = map.find(_._2 == 'S').get._1
        val (newMap, loop) = findLoop(map, startingPoint)
        val maxX           = newMap.keys.maxBy(_.x).x
        val maxY           = newMap.keys.maxBy(_.y).y

        val loopSet                    = loop.toSet
        def loopForever: LazyList[Pos] = LazyList.from(loop) lazyAppendedAll loopForever

        val firstWall = (for {
            x <- 0 to loopSet.maxBy(_.x).x
            y <- 0 to loopSet.maxBy(_.y).y
        } yield Pos(x, y)).find(loopSet.contains).get

        val adjustedLoop = loopForever.dropWhile(_ != firstWall)

        val (outsideIsToThe, movingTowards) = if (adjustedLoop.head.x == adjustedLoop(1).x) {
            (OutsideIsToThe.Right, MovingTowards.Down)
        } else {
            (OutsideIsToThe.Left, MovingTowards.Right)
        }

        def searchUntilWallHit(toSearch: List[Pos], outerPoints: Set[Pos]): Set[Pos] =
            toSearch.headOption match
                case Some(current) =>
                    if (loopSet.contains(current)) searchUntilWallHit(toSearch.tail, outerPoints)
                    else if (current.x < 0 || current.x > maxX || current.y < 0 || current.y > maxY)
                        searchUntilWallHit(toSearch.tail, outerPoints)
                    else
                        val next = current.neighbors.filterNot(outerPoints.contains)
                        searchUntilWallHit(next ++ toSearch.tail, outerPoints + current)
                case None => outerPoints

        def search(
            positions: LazyList[Pos],
            outsideIsToThe: OutsideIsToThe,
            movingTowards: MovingTowards,
            remaining: Int,
            outerPoints: Set[Pos]
        ): Set[Pos] =
            if (remaining == 0) outerPoints
            else
                val current = positions.head
                val next    = positions.tail.head

                val newMovingTowards = movingTowards match
                    case MovingTowards.Up =>
                        if (next.x == current.x) MovingTowards.Up
                        else if (next.x < current.x) MovingTowards.Left
                        else if (next.x > current.x) MovingTowards.Right
                        else ???

                    case MovingTowards.Down =>
                        if (next.x == current.x) MovingTowards.Down
                        else if (next.x < current.x) MovingTowards.Left
                        else if (next.x > current.x) MovingTowards.Right
                        else ???

                    case MovingTowards.Right =>
                        if (next.y == current.y) MovingTowards.Right
                        else if (next.y < current.y) MovingTowards.Up
                        else if (next.y > current.y) MovingTowards.Down
                        else ???

                    case MovingTowards.Left =>
                        if (next.y == current.y) MovingTowards.Left
                        else if (next.y < current.y) MovingTowards.Up
                        else if (next.y > current.y) MovingTowards.Down
                        else ???

                val immediateNewOuterPoints = (outsideIsToThe, movingTowards) match
                    case (OutsideIsToThe.Left, MovingTowards.Right) =>
                        newMovingTowards match
                            case MovingTowards.Right => List(current.copy(y = current.y - 1))
                            case MovingTowards.Up    => List(current.copy(y = current.y - 1))
                            case MovingTowards.Down =>
                                List(current.copy(y = current.y - 1), current.copy(x = current.x + 1))
                            case _ => ???

                    case (OutsideIsToThe.Left, MovingTowards.Left) =>
                        newMovingTowards match
                            case MovingTowards.Left => List(current.copy(y = current.y + 1))
                            case MovingTowards.Up =>
                                List(current.copy(y = current.y + 1), current.copy(x = current.x - 1))
                            case MovingTowards.Down => List(current.copy(y = current.y + 1))
                            case _                  => ???

                    case (OutsideIsToThe.Left, MovingTowards.Up) =>
                        newMovingTowards match
                            case MovingTowards.Up => List(current.copy(x = current.x - 1))
                            case MovingTowards.Right =>
                                List(current.copy(x = current.x - 1), current.copy(y = current.y - 1))
                            case MovingTowards.Left => List(current.copy(x = current.x - 1))
                            case _                  => ???
                    case (OutsideIsToThe.Left, MovingTowards.Down) =>
                        newMovingTowards match
                            case MovingTowards.Down  => List(current.copy(x = current.x + 1))
                            case MovingTowards.Right => List(current.copy(x = current.x + 1))
                            case MovingTowards.Left =>
                                List(current.copy(x = current.x + 1), current.copy(y = current.y + 1))
                            case _ => ???

                    case (OutsideIsToThe.Right, MovingTowards.Right) =>
                        newMovingTowards match
                            case MovingTowards.Right => List(current.copy(y = current.y + 1))
                            case MovingTowards.Up =>
                                List(current.copy(y = current.y + 1), current.copy(x = current.x + 1))
                            case MovingTowards.Down => List(current.copy(y = current.y + 1))
                            case _                  => ???

                    case (OutsideIsToThe.Right, MovingTowards.Left) =>
                        newMovingTowards match
                            case MovingTowards.Left => List(current.copy(y = current.y - 1))
                            case MovingTowards.Up =>
                                List(current.copy(y = current.y - 1))
                            case MovingTowards.Down =>
                                List(current.copy(y = current.y - 1), current.copy(x = current.x - 1))
                            case _ => ???

                    case (OutsideIsToThe.Right, MovingTowards.Up) =>
                        newMovingTowards match
                            case MovingTowards.Up => List(current.copy(x = current.x + 1))
                            case MovingTowards.Right =>
                                List(current.copy(x = current.x + 1))
                            case MovingTowards.Left =>
                                List(current.copy(x = current.x + 1), current.copy(y = current.y - 1))
                            case _ => ???

                    case (OutsideIsToThe.Right, MovingTowards.Down) =>
                        newMovingTowards match
                            case MovingTowards.Down => List(current.copy(x = current.x - 1))
                            case MovingTowards.Right =>
                                List(current.copy(x = current.x - 1), current.copy(y = current.y + 1))
                            case MovingTowards.Left => List(current.copy(x = current.x - 1))
                            case _                  => ???

                val newOuterPoints = searchUntilWallHit(immediateNewOuterPoints, outerPoints)

                search(positions.tail, outsideIsToThe, newMovingTowards, remaining - 1, newOuterPoints)

        val outerPoints = search(adjustedLoop, outsideIsToThe, movingTowards, loopSet.size, Set.empty)

        val pathVisual = (for {
            y <- 0 to newMap.keys.maxBy(_.y).y
            x <- 0 to newMap.keys.maxBy(_.x).x
        } yield {
            val pos = Pos(x, y)
            if (outerPoints.contains(pos)) '!'
            else if (loopSet.contains(pos)) newMap(pos)
            else '*'
        }).grouped(newMap.keys.maxBy(_.x).x + 1).map(_.mkString).mkString("\n").map {
            case 'F' => '╔'
            case '7' => '╗'
            case 'J' => '╝'
            case 'L' => '╚'
            case '|' => '║'
            case '-' => '═'
            case c   => c
        }

        println(pathVisual)

        (maxX + 1) * (maxY + 1) - outerPoints.size - loopSet.size
