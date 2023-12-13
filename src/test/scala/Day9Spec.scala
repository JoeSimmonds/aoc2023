class Day9Spec extends munit.FunSuite {
  test("provided example") {
    val input = """0 3 6 9 12 15
    |1 3 6 10 15 21
    |10 13 16 21 30 45
    |""".stripMargin

    assertEquals(day9.sumOfNextValues(input), 114L)
  }

  test("projections from provided examples") {
    assertEquals(day9.projectNext(List(10, 13, 16, 21, 30, 45)),  68L)
    assertEquals(day9.projectNext(List(1, 3, 6, 10, 15, 21)),  28L)
    assertEquals(day9.projectNext(List(10, 13, 16, 21, 30, 45)),  68L)

  }

  test("Parser can parse file") {
    val input = """6 7 145 89
    |56 76 89 76 8
    |""".stripMargin

    val result = day9.Day9Parser.parse(day9.Day9Parser.dataFile, input).get
    assertEquals(result.size, 2)
    assertEquals(result(0), List(6L, 7L, 145L, 89L))
    assertEquals(result(1), List(56L, 76L, 89L, 76L, 8L))
  }

  test("can project next for constant series") {
    assertEquals(day9.projectNext(List(3L, 3L, 3L, 3L)), 3L)
  }

}
