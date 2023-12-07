package aoc

import aoc.Common.timed

import scala.io.Source
import Day7.HandType.*

object Day7:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day7.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    val cardOrder = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A').zipWithIndex.toMap
    val cardOrdering = new Ordering[Char]:
        override def compare(x: Char, y: Char): Int =
            cardOrder(x) - cardOrder(y)

    val cardOrder2 = List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A').zipWithIndex.toMap
    val cardOrdering2 = new Ordering[Char]:
        override def compare(x: Char, y: Char): Int =
            cardOrder2(x) - cardOrder2(y)

    enum HandType:
        case FiveKind, FourKind, FullHouse, ThreeKind, TwoPair, OnePair, HighCard

    val typeOrder =
        List(FiveKind, FourKind, FullHouse, ThreeKind, TwoPair, OnePair, HighCard).reverse.zipWithIndex.toMap
    val typeOrdering = new Ordering[HandType]:
        override def compare(x: HandType, y: HandType): Int =
            typeOrder(x) - typeOrder(y)

    def handToType(hand: List[Char]): HandType =
        val grouped = hand.groupBy(identity).view.mapValues(_.size).toVector
        if (grouped.size == 1) {
            FiveKind
        } else if (grouped.size == 2) {
            if (grouped.exists(_._2 == 4)) {
                FourKind
            } else {
                FullHouse
            }
        } else if (grouped.size == 3) {
            if (grouped.exists(_._2 == 3)) {
                ThreeKind
            } else {
                TwoPair
            }
        } else if (grouped.size == 4) {
            OnePair
        } else {
            HighCard
        }

    def handToType2(hand: List[Char]): HandType =
        val grouped = hand.groupBy(identity).view.mapValues(_.size)
        if (grouped.contains('J')) {
            grouped.toList.filter(_._1 != 'J').maxByOption(_._2) match
                case Some((replacement, _)) => handToType(hand.map(c => if (c == 'J') replacement else c))
                case None                   => handToType(hand)
        } else {
            handToType(hand)
        }

    val handOrder = new Ordering[List[Char]]:
        override def compare(x: List[Char], y: List[Char]): Int =
            val xType = handToType(x)
            val yType = handToType(y)
            val res = if (xType == yType) {
                x.zip(y).map((x, y) => cardOrdering.compare(x, y)).find(_ != 0).getOrElse(0)
            } else {
                typeOrdering.compare(xType, yType)
            }
            println(s"X: ${x.mkString(" ")} Y: ${y.mkString(" ")} XType: $xType YType: $yType Res: $res")

            res

    val handOrder2 = new Ordering[List[Char]]:
        override def compare(x: List[Char], y: List[Char]): Int =
            val xType = handToType2(x)
            val yType = handToType2(y)
            val res = if (xType == yType) {
                x.zip(y).map((x, y) => cardOrdering2.compare(x, y)).find(_ != 0).getOrElse(0)
            } else {
                typeOrdering.compare(xType, yType)
            }
            println(s"X: ${x.mkString(" ")} Y: ${y.mkString(" ")} XType: $xType YType: $yType Res: $res")

            res

    def part1(lines: List[String]): Long =
        lines
            .map { case s"$cards $bid" =>
                (cards.toCharArray.toList, bid.trim.toInt)
            }
            .sortBy(_._1)(handOrder)
            .zipWithIndex
            .map { case ((hand, bid), rank) =>
                println(s"Hand: ${hand.mkString(" ")} Bid: $bid Rank: ${rank + 1}")
                bid * (rank + 1)
            }
            .sum

    def part2(lines: List[String]): Long =
        lines
            .map { case s"$cards $bid" =>
                (cards.toCharArray.toList, bid.trim.toInt)
            }
            .sortBy(_._1)(handOrder2)
            .zipWithIndex
            .map { case ((hand, bid), rank) =>
                println(s"Hand: ${hand.mkString(" ")} Bid: $bid Rank: ${rank + 1}")
                bid * (rank + 1)
            }
            .sum
