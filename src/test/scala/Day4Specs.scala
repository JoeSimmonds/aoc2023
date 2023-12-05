class Day4Specs extends munit.FunSuite {
  test("A card with no winners scores 0") {
    val input = "Card 34: 97 67 53 | 54 28 78"
    assertEquals(Day4.totalScore(List(input)), 0)
  }

  test("A card with one winner scores 1") {
    val input = "Card 34: 97 78 53 | 54 28 78"
    assertEquals(Day4.totalScore(List(input)), 1)
  }

  test("A card with 5 winners scores 16") {
    val input = "Card 34: 97 78 53 45 89 | 53 45 89 78 97"
    assertEquals(Day4.totalScore(List(input)), 16)
  }

  test("provided example of score") {
    val input = List(
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
        "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
        "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
        "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
        "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
        "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    )

    assertEquals(Day4.totalScore(input), 13)
  }

  test("provided example of proliferation") {
    val input = List(
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
        "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
        "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
        "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
        "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
        "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    )

    assertEquals(Day4.proliferationCount(input), 30)
  }
}
