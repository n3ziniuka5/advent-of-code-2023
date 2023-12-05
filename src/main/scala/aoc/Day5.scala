package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.io.Source

object Day5:
    case class Range(source: Long, destination: Long, size: Long)
    case class Data(seeds: List[Long], maps: MultiDict[String, Range])
    object Data:
        val empty: Data = Data(Nil, MultiDict.empty)

    def parse(lines: List[String]): Data =
        val seeds = lines.head.drop("seeds: ".length).split(" ").map(_.trim.toLong).toList

        @tailrec
        def loop(lines: List[String], currentMap: String, maps: MultiDict[String, Range]): MultiDict[String, Range] =
            if lines.isEmpty then maps
            else if lines.head.isEmpty then loop(lines.tail, currentMap, maps)
            else
                lines.head match
                    case s"$map map:" => loop(lines.tail, map, maps)
                    case l =>
                        val Array(dest, src, range) = l.split(" ").map(_.trim.toLong)
                        val r                       = Range(src, dest, range)
                        loop(lines.tail, currentMap, maps.add(currentMap, r))

        val maps = loop(lines.drop(2), "", MultiDict.empty)
        Data(seeds, maps)

    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day5.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val data = parse(lines)
        data.seeds.map { seed =>
            def getRange(name: String, number: Long): Long =
                data.maps
                    .get(name)
                    .find { r =>
                        (r.source <= number) && ((r.source + r.size) >= number)
                    }
                    .map { r =>
                        r.destination + (number - r.source)
                    }
                    .getOrElse(number)

            getRange(
              "humidity-to-location",
              getRange(
                "temperature-to-humidity",
                getRange(
                  "light-to-temperature",
                  getRange(
                    "water-to-light",
                    getRange("fertilizer-to-water", getRange("soil-to-fertilizer", getRange("seed-to-soil", seed)))
                  )
                )
              )
            )
        }.min

    def part2(lines: List[String]): Long =
        val data = parse(lines)

        def getRange(name: String, t: (Long, Long)): (Long, Long) =
            val (number, skippedLastTime) = t
            data.maps
                .get(name)
                .find { r =>
                    (r.source <= number) && ((r.source + r.size) >= number)
                }
                .map { r =>
                    val destination = r.destination + (number - r.source)
                    val canSkip     = r.source + r.size - number
                    (destination, math.min(canSkip, skippedLastTime))
                }
                .getOrElse {
                    val canSkip = data.maps
                        .get(name)
                        .map { m =>
                            m.source - number - 1
                        }
                        .filter(_ >= 0)
                        .minOption
                        .getOrElse(Long.MaxValue)
                    (number, math.min(canSkip, skippedLastTime))
                }

        def loop(seeds: List[(Long, Long)], currentSeed: Long, min: Long): Long =
            val currentRange = seeds.head
            if (currentSeed > currentRange._1 + currentRange._2) {
                if (seeds.tail.isEmpty) min
                else loop(seeds.tail, seeds.tail.head._1, min)
            } else {
                val (location, canSkip) = getRange(
                  "humidity-to-location",
                  getRange(
                    "temperature-to-humidity",
                    getRange(
                      "light-to-temperature",
                      getRange(
                        "water-to-light",
                        getRange(
                          "fertilizer-to-water",
                          getRange("soil-to-fertilizer", getRange("seed-to-soil", (currentSeed, Long.MaxValue)))
                        )
                      )
                    )
                  )
                )

                loop(seeds, currentSeed + math.max(1, canSkip), math.min(min, location))
            }

        loop(data.seeds.sliding(2, 2).map(l => (l.head, l(1))).toList, data.seeds.head, Long.MaxValue)
