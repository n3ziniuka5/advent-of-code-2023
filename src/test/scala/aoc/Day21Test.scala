package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day21Test extends ZIOSpecDefault:
    val input = List(
      "...........",
      ".....###.#.",
      ".###.##..#.",
      "..#.#...#..",
      "....#.#....",
      ".##..S####.",
      ".##..#...#.",
      ".......##..",
      ".##.#.####.",
      ".##..##.##.",
      "...........",
    )

    override def spec = suite("Day 21")(
      test("Part 1")(assertTrue(Day21.part1(input, 6) == 16)),
      // test("Part 2-1")(assertTrue(Day21.part2(input, 50) == 1594)),
      // test("Part 2-2")(assertTrue(Day21.part2(input, 100) == 6536)),
      // test("Part 2-3")(assertTrue(Day21.part2(input, 500) == 167004)),
      test("Part 2-4")(assertTrue(Day21.part2(input, 1000) == 668697)),
      // test("Part 2-5")(assertTrue(Day21.part2(input, 5000) == 16733044))
    )
