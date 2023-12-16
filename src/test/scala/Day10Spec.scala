class Day10Spec extends munit.FunSuite {
    test("Can identify the loop") {
        val input = List(
            ".....",
            ".S-7.",
            ".|.|.",
            ".L-J.",
            "....."
        )

        val expectedList = List(
            (1, 1),(2, 1), (3, 1),
            (1, 2),(3, 2),
            (1, 3),(2, 3), (3, 3)
        )

        val SUT = day10.PipeMap(input)
        assertEquals(SUT.identifyLoop().map(_.asTuple).sorted, expectedList.sorted)
    }

    test("provided example") {
        val input = List(
            "7-F7-",
            ".FJ|7",
            "SJLL7",
            "|F--J",
            "LJ.LJ"
        )

        val SUT = day10.PipeMap(input)
        assertEquals(SUT.furthestDistanceInLoop(), 8)
    }

        test("Part 2 provided example") {
        val input = List(
            ".F----7F7F7F7F-7....",
            ".|F--7||||||||FJ....",
            ".||.FJ||||||||L7....",
            "FJL7L7LJLJ||LJ.L-7..",
            "L--J.L7-..LJS7F-7L7.",
            "....F-J..F7FJ|L7L7L7",
            "L...L7.F7||L7|.L7L7|",
            ".....|FJLJ|FJ|F7|.LJ",
            "....FJL-7.||.||||...",
            "....L---J.LJ.LJLJ..."
        )

        val SUT = day10.PipeMap(input)
        assertEquals(SUT.tilesInside(), 8)
    }
}
