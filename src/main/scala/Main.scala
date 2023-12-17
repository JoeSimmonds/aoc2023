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
  println("Skipping Day 5 Part 2 as the solution takes 6 seconds to run")
  // val d5p2 = day5.Almanac.parseV2FromString(day5Data).getClosestLocation()
  // println(s"Day5 Part2 : $d5p2")

  val day6Data = Source.fromResource("Day6.data.txt").getLines().mkString(sys.props("line.separator"))
  val d6p1 = day6.RaceList.productOfWins(day6Data)
  println(s"Day6 Part1 : $d6p1")
  val d6p2 = day6.RaceList.winCountForSingleRace(day6Data)
  println(s"Day6 Part2 : $d6p2")

  val day7Data = Source.fromResource("Day7.data.txt").getLines().mkString(sys.props("line.separator"))
  val d7p1 = day7.getTotalScoreFromAllHands(day7Data)
  println(s"Day7 Part1 : $d7p1")
  val d7p2 = day7.getTotalScoreFromAllHandsWithJokers(day7Data)
  println(s"Day7 Part2 : $d7p2")

  val day8Data = Source.fromResource("Day8.data.txt").getLines().mkString(sys.props("line.separator"))
  val d8p1 = day8.countSteps(day8Data)
  println(s"Day8 Part1 : $d8p1")
  println("Skipping Day 8 Part 2 as the solution takes 2hrs+")
  // val d8p2 = day8.countStepsForGhosts(day8Data)  // takes about 2 hrs to run to completion
  // println(s"Day8 Part2 : $d8p2")

  val day9Data = Source.fromResource("Day9.data.txt").getLines().mkString(sys.props("line.separator"))
  val d9p1 = day9.sumOfNextValues(day9Data)
  println(s"Day9 Part1 : $d9p1")
  val d9p2 = day9.sumOfPreviousValues(day9Data)
  println(s"Day9 Part2 : $d9p2")

  val day10Data = Source.fromResource("Day10.data.txt").getLines().toList
  val pipeMap = day10.PipeMap(day10Data)
  val d10p1 = pipeMap.furthestDistanceInLoop()
  println(s"Day10 Part1 : $d10p1")
  val d10p2 = pipeMap.tilesInside()
  println(s"Day10 Part2 : $d10p2")

  val day11Data = Source.fromResource("Day11.data.txt").getLines().toList
  val d11p1 = day11.totalInterGalacticDistance(day11Data, 2)
  println(s"Day11 Part1 : $d11p1")
  val d11p2 = day11.totalInterGalacticDistance(day11Data, 1000000)
  println(s"Day11 Part2 : $d11p2")


