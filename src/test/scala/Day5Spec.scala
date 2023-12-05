import day5._

class Day5Spec extends munit.FunSuite {
  test("SeedRange is correct") {
    val SUT = SeedRange(18, 1)
    assert(SUT.isInRange(18))
  }

  test("Almanac can check list of ranges correctly") {
    val data = Almanac(
      List(SeedRange(15, 1)),
      RangeMap("seed", "location", List(MappedRange(3, 15, 1)))
    )
    assert(data.isInAnySeedRange(15))
  }

  test("Given a simple map and a single seed seed the location") {
    val data = Almanac(
      List(SeedRange(15, 1)),
      RangeMap("seed", "location", List(MappedRange(15, 3, 1)))
    )

    assertEquals(data.getClosestLocation(), Some(3L))
  }

  test("Given a map with a range and a single seed finds the location") {
    val data = Almanac(
      List(SeedRange(18, 1)),
      RangeMap("seed", "location", List(MappedRange(15, 3, 5)))
    )

    assertEquals(data.getClosestLocation(), Some(6L))
  }

  test(
    "Given a map with multiple ranges and a single seed finds the location"
  ) {
    val data = Almanac(
      List(SeedRange(41, 1)),
      RangeMap(
        "seed",
        "location",
        List(
          MappedRange(15, 3, 5),
          MappedRange(32, 12, 10)
        )
      )
    )

    assertEquals(data.getClosestLocation(), Some(21L))
  }

  test(
    "Given a map with multiple ranges and a multiple seeds finds the lowest location"
  ) {
    val data = Almanac(
      List(41, 35).map(SeedRange.apply(_, 1)),
      RangeMap(
        "seed",
        "location",
        List(
          MappedRange(15, 3, 5),
          MappedRange(32, 12, 10)
        )
      )
    )

    assertEquals(data.getClosestLocation(), Some(15L))
  }

  test("unmapped values are used as-is") {
    val data = Almanac(
      List(SeedRange(1, 1)),
      RangeMap("seed", "location", List(MappedRange(3, 15, 1)))
    )

    assertEquals(data.getClosestLocation(), Some(1L))
  }

  test("Maps can be combined") {
    val m1 = RangeMap("a", "b", List(MappedRange(10, 20, 10)))
    val m2 = RangeMap("b", "c", List(MappedRange(20, 30, 5)))

    val result: RangeMap = m1 andThen m2
    assertEquals(result.source, "a")
    assertEquals(result.destination, "c")
    assertEquals(result(9), 9L)
    assertEquals(result(10), 30L)
    assertEquals(result(11), 31L)
    assertEquals(result(12), 32L)
    assertEquals(result(13), 33L)
    assertEquals(result(14), 34L)
    assertEquals(result(15), 25L)
    assertEquals(result(16), 26L)
    assertEquals(result(17), 27L)
    assertEquals(result(18), 28L)
    assertEquals(result(19), 29L)
    assertEquals(result(20), 30L)
    assertEquals(result(21), 31L)
    assertEquals(result(22), 32L)
    assertEquals(result(23), 33L)
    assertEquals(result(24), 34L)
    assertEquals(result(25), 25L)
    assertEquals(result(200), 200L)
  }

  test("The provided example with parsing") {
    val input = """
    |seeds: 79 14 55 13
    |
    |seed-to-soil map:
    |50 98 2
    |52 50 48
    |
    |soil-to-fertilizer map:
    |0 15 37
    |37 52 2
    |39 0 15
    |
    |fertilizer-to-water map:
    |49 53 8
    |0 11 42
    |42 0 7
    |57 7 4
    |
    |water-to-light map:
    |88 18 7
    |18 25 70
    |
    |light-to-temperature map:
    |45 77 23
    |81 45 19
    |68 64 13
    |
    |temperature-to-humidity map:
    |0 69 1
    |1 0 69
    |
    |humidity-to-location map:
    |60 56 37
    |56 93 4
    |""".stripMargin

    val SUT = Almanac.parseFromString(input)
    assertEquals(SUT.getClosestLocation(), Some(35L))
  }

  test("Parser can parse Almanac with a single Map") {
    val input = """
    |seeds: 678 45 8237 87
    |
    |seed-to-location map:
    |50 98 2
    |52 50 48
    |
    """.stripMargin

    val result: Almanac = Almanac.parseFromString(input)
    val expectedSeeds = List(678L, 45L, 8237L, 87L).map(SeedRange.apply(_, 1))
    assert(clue(result.seeds).sameElements(clue(expectedSeeds)))
    assert(clue(result.map.source) == "seed")
    assert(clue(result.map.destination) == "location")
    assertEquals(result.map(98), 50L)
    assertEquals(result.map(50), 52L)
    assertEquals(result.map(51), 53L)
  }

    test("Parser can parse Almanac with multiple maps") {
    val input = """
    |seeds: 678 45 8237 87
    |
    |seed-to-humidity map:
    |98 50 2
    |50 52 48
    |
    |humidity-to-location map:
    |1000 98 2
    |2000 50 48
    | 
    |
    """.stripMargin

    val result: Almanac = Almanac.parseFromString(input)
    val expectedSeeds = List(678L, 45L, 8237L, 87L).map(SeedRange.apply(_, 1))
    assert(clue(result.seeds).sameElements(clue(expectedSeeds)))
    assert(clue(result.map.source) == "seed")
    assert(clue(result.map.destination) == "location")
    assertEquals(result.map(50), 1000L)
    assertEquals(result.map(51), 1001L)
    assertEquals(result.map(52), 2000L)
    assertEquals(result.map(53), 2001L)
  }


  test("The provided example with parsing for part 2") {
    val input = """
    |seeds: 79 14 55 13
    |
    |seed-to-soil map:
    |50 98 2
    |52 50 48
    |
    |soil-to-fertilizer map:
    |0 15 37
    |37 52 2
    |39 0 15
    |
    |fertilizer-to-water map:
    |49 53 8
    |0 11 42
    |42 0 7
    |57 7 4
    |
    |water-to-light map:
    |88 18 7
    |18 25 70
    |
    |light-to-temperature map:
    |45 77 23
    |81 45 19
    |68 64 13
    |
    |temperature-to-humidity map:
    |0 69 1
    |1 0 69
    |
    |humidity-to-location map:
    |60 56 37
    |56 93 4
    |""".stripMargin

    val SUT = Almanac.parseV2FromString(input)
    assertEquals(SUT.getClosestLocation(500L), Some(46L))
  }
}
