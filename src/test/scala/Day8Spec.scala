
class Day8Spec extends munit.FunSuite {
  test("provided example") {
    val input = """LLR
    |
    |AAA = (BBB, BBB)
    |BBB = (AAA, ZZZ)
    |ZZZ = (ZZZ, ZZZ)
    |""".stripMargin

    assertEquals(day8.countSteps(input), 6L)
  }

  test("can parse a node line") {
    val input = "AAA = (VVV, GGG)"
    val p = new day8.NetworkParser
    val x = p.parse(p.node, input)

    assertEquals(x.get, ("AAA", ("VVV", "GGG")))
  }

  test("can parse a plan") {
    val input = """LR
    |
    |11A = (11B, XXX)
    |11B = (XXX, 11Z)
    |11Z = (11B, XXX)
    |22A = (22B, XXX)
    |22B = (22C, 22C)
    |22C = (22Z, 22Z)
    |22Z = (22B, 22B)
    |XXX = (XXX, XXX)
    |""".stripMargin

    val p = new day8.NetworkParser
    assert(p.parse(p.plan, input).successful)
  }

  test("part2 provided example") {
    val input = """LR
    |
    |11A = (11B, XXX)
    |11B = (XXX, 11Z)
    |11Z = (11B, XXX)
    |22A = (22B, XXX)
    |22B = (22C, 22C)
    |22C = (22Z, 22Z)
    |22Z = (22B, 22B)
    |XXX = (XXX, XXX)
    |""".stripMargin

    assertEquals(day8.countStepsForGhosts(input), 6L)
  }


  test("stepsUntil next Z") {
    val input = """LR
    |
    |11A = (11B, XXX)
    |11B = (XXX, 11Z)
    |11Z = (11B, XXX)
    |22A = (22B, XXX)
    |22B = (22C, 22C)
    |22C = (22Z, 22Z)
    |22Z = (22B, 22B)
    |XXX = (XXX, XXX)
    |""".stripMargin

    val p = new day8.NetworkParser
    val plan = p.parse(p.plan, input).get

    assertEquals(plan.stepsUntilNextNodeEndingWithZ(plan.SearchState(0, "11A", 0), 0).stepsTaken, 2L)
    assertEquals(plan.stepsUntilNextNodeEndingWithZ(plan.SearchState(0, "22A", 0), 0).stepsTaken, 3L)
  }
}
