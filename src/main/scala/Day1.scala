import cats._
import cats.data._
import cats.syntax.all._
import cats.syntax.distributive

object Day1Part1 {
  private def getNumberForLine(str: String): Option[Int] = {
   val digits = str.filter(_.isDigit)
   (digits.headOption, digits.lastOption)
      .mapN((a, b) => s"$a$b")
      .flatMap(_.toIntOption)
  }

  def getPart1Sum(inputs: List[String]): Int = inputs.map(getNumberForLine).flatten.sum
}

object Day1Part2 {
  private val digitStrings= List(
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "1", "2", "3", "4", "5", "6", "7", "8", "9"
  )

  private def findLeftMostDigit(str: String): Option[Int] = 
    digitStrings
      .map(dstr => dstr -> str.indexOf(dstr))
      .filter(_._2 >= 0)
      .minByOption(_._2)
      .map(_._1)
      .map(dStr => digitStrings.indexOf(dStr) % 9 +1)


  private def findRightMostDigit(str: String): Option[Int] = 
    digitStrings
      .map(dstr => dstr -> str.lastIndexOf(dstr))
      .filter(_._2 >= 0)
      .maxByOption(_._2)
      .map(_._1)
      .map(dStr => digitStrings.indexOf(dStr) % 9 +1)

  private def getNumberForLineWithStringBasedDigits(line: String): Option[Int] ={
    (findLeftMostDigit(line), findRightMostDigit(line)) match
      case (None, None) => None
      case (Some(a), None) => Some(a*10 + a)
      case (None, Some(b)) => Some(b*10 + b)
      case (Some(a), Some(b)) => Some(a*10 + b)
  }

  def getPart2Sum(inputs: List[String]): Int = inputs.map(getNumberForLineWithStringBasedDigits).flatten.sum
}
