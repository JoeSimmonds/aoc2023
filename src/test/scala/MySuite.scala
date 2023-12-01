// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Day 1 Part 1 should get the value from a single line that has no Alphas") {
    val input = "23" 
    assertEquals (Day1Part1.getPart1Sum(List(input)), 23)
  }

  test("Day 1 Part 1 should get the value from a single line ignoring alphas") {
    val input = "FGDFDHGF2dfgldkjdl7fdkgjhfdgkj" 
    assertEquals (Day1Part1.getPart1Sum(List(input)), 27)
  }

  test("Day 1 Part 1 should get the value from a single line ignoring alphas and middle digits") {
    val input = "FGDFDHGF4dfgld7kj6dl3fdkgjhfdgkj" 
    assertEquals (Day1Part1.getPart1Sum(List(input)), 43)
  }

  test("Day 1 Part 1 should get the value from a single line when  there's only 1 digit") {
    val input = "LJUhjcbdiluvchsdkb5KUHKJH" 
    assertEquals (Day1Part1.getPart1Sum(List(input)), 55)
  }

  test("Day 1 Part 1 should add multple lines together") {
    val input = List(
      "23",
      "FGDFDHGF2dfgldkjdl7fdkgjhfdgkj", 
      "FGDFDHGF4dfgld7kj6dl3fdkgjhfdgkj", 
      "LJUhjcbdiluvchsdkb5KUHKJH",
    )

    assertEquals (Day1Part1.getPart1Sum(input), 148)
  }

  test("Day 1 Part 1 provided example") {
    val input = List(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet"
    )

    assertEquals (Day1Part1.getPart1Sum(input), 142)
  }

  test("Day 1 Part 2 Should detect names of digits and treat those as digits") {
    val input = "FGDFDHGFsevendfgldkj5sjklhkjl8dlsixfdkgninejhfdgkj" 
    assertEquals (Day1Part2.getPart2Sum(List(input)), 79)
  }

  test("Day 1 part 2 should not be fooled by overlapping numbers in the left digit") {
    val input = "eightwothree"
    assertEquals (Day1Part2.getPart2Sum(List(input)), 83)
  }

  test("Day 1 part 2 Day 1 part 2 should not be fooled by overlapping numbers in the right digit") {
    val input = "eightwockldscl34keighthree"
    assertEquals (Day1Part2.getPart2Sum(List(input)), 83)
  }

  test("Day 1 Part 2 should add multple lines together") {
    val input = List(
      "23",
      "FGDFDHGF2dfgldkjdl7fdkgjhfdgkj", 
      "FGDFDHGF4dfgld7kj6dl3fdkgjhfdgkj", 
      "LJUhjcbdiluvchsdkb5KUHKJH",
      "FGDFDHGFsevendfgldkj5sjklhkjl8dlsixfdkgjhfdgkj" 
    )

    assertEquals (Day1Part2.getPart2Sum(input), 23 + 27 + 43 + 55 + 76)
  }

  test("Day 1 Part 2 provided example") {
    val input = List(
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    )

    assertEquals (Day1Part2.getPart2Sum(input), 281)
  }
  

}
