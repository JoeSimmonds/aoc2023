class Day11Spec extends munit.FunSuite {
    test("Provided Example") {
        val input = List(
            "...#......",
            ".......#..",
            "#.........",
            "..........",
            "......#...",
            ".#........",
            ".........#",
            "..........",
            ".......#..",
            "#...#....."
        )

        assertEquals(day11.totalInterGalacticDistance(input, 2), 374)
        assertEquals(day11.totalInterGalacticDistance(input, 10), 1030)
        assertEquals(day11.totalInterGalacticDistance(input, 100), 8410)
    }
}
