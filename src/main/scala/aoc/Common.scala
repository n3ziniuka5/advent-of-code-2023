package aoc

import scala.collection.mutable

object Common:
    def timed[A](label: String, f: => A): Unit =
        val start = System.currentTimeMillis()
        val res   = f
        val end   = System.currentTimeMillis()

        println(s"$label answer - $res It took ${end - start}ms")

    extension [A](q: mutable.PriorityQueue[A])
        def enqueueAndKeepMaxSize(element: A, maxSize: Int): mutable.PriorityQueue[A] =
            q.enqueue(element)
            if (q.size > maxSize) {
                q.dequeue()
            }

            q

case class Point(x: Int, y: Int):
    def up: Point    = Point(x, y - 1)
    def down: Point  = Point(x, y + 1)
    def left: Point  = Point(x - 1, y)
    def right: Point = Point(x + 1, y)

    def topLeft: Point     = Point(x - 1, y - 1)
    def topRight: Point    = Point(x + 1, y - 1)
    def bottomLeft: Point  = Point(x - 1, y + 1)
    def bottomRight: Point = Point(x + 1, y + 1)

    def adjacent = List(up, down, left, right)
    def adjacentDiagonal = for {
        x <- List(-1, 0, 1)
        y <- List(-1, 0, 1) if x != 0 || y != 0
    } yield Point(this.x + x, this.y + y)

    def inBounds(map: Map2d[_]): Boolean =
        x >= 0 && x <= map.maxX && y >= 0 && y <= map.maxY

case class Map2d[V](underlying: Map[Point, V]):
    lazy val maxX = underlying.keys.map(_.x).max
    lazy val maxY = underlying.keys.map(_.y).max

    def map[V2](f: ((Point, V)) => (Point, V2)): Map2d[V2] = Map2d(underlying.map(f))

    def apply(k: Point): V = underlying(k)

    def get(k: Point): Option[V] = underlying.get(k)

object Map2d:
    def fromLines(lines: List[String]): Map2d[Char] =
        val underlying = lines.zipWithIndex.flatMap { (line, y) =>
            line.zipWithIndex.map { (char, x) =>
                Point(x, y) -> char
            }
        }.toMap

        Map2d(underlying)
