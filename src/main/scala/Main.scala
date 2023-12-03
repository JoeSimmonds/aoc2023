import scala.io.Source


@main def main: Unit =
  val lines = Source.fromResource("Day1.data.txt").getLines().toList
  val d1p1 = Day1Part1.getPart1Sum(lines)
  val d1p2 = Day1Part2.getPart2Sum(lines)
  println(s"Day1 Part1 : $d1p1")
  println(s"Day1 Part2 : $d1p2")
  val linesDay2 = Source.fromResource("Day2.data.txt").getLines().toList
  val d2p1 = Day2.getSum(linesDay2)
  val d2p2 = Day2.getSumOfPowerOfAllGames(lines = linesDay2)
  println(s"Day2 Part1 : $d2p1")
  println(s"Day2 Part2 : $d2p2")
  

