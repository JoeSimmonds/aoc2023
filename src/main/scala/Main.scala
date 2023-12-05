import scala.io.Source


@main def main: Unit =
  val linesDay1 = Source.fromResource("Day1.data.txt").getLines().toList
  val d1p1 = Day1Part1.getPart1Sum(linesDay1)
  val d1p2 = Day1Part2.getPart2Sum(linesDay1)
  println(s"Day1 Part1 : $d1p1")
  println(s"Day1 Part2 : $d1p2")

  val linesDay2 = Source.fromResource("Day2.data.txt").getLines().toList
  val d2p1 = Day2.getSum(linesDay2)
  val d2p2 = Day2.getSumOfPowerOfAllGames(lines = linesDay2)
  println(s"Day2 Part1 : $d2p1")
  println(s"Day2 Part2 : $d2p2")

  val linesDay3 = Source.fromResource("Day3.data.txt").getLines().toList
  val d3p1 = new day3.Schematic(linesDay3).sumOfPartNumbers()
  val d3p2 = new day3.Schematic(linesDay3).totalGearRatio()
  println(s"Day3 Part1 : $d3p1")
  println(s"Day3 Part2 : $d3p2")

  val linesDay4 = Source.fromResource("Day4.data.txt").getLines().toList
  val d4p1 = Day4.totalScore(linesDay4)
  val d4p2 = Day4.proliferationCount(linesDay4)
  println(s"Day4 Part1 : $d4p1")
  println(s"Day4 Part2 : $d4p2")

  val day5Data = Source.fromResource("Day5.data.txt").getLines().mkString(sys.props("line.separator"))
  val d5p1 = day5.Almanac.parseFromString(day5Data).getClosestLocation()
  println(s"Day5 Part1 : $d5p1")
  val d5p2 = day5.Almanac.parseV2FromString(day5Data).getClosestLocation()
  println(s"Day5 Part2 : $d5p2")

