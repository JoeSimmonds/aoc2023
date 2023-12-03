import scala.collection.immutable.Map

sealed trait Colour

object Colour {
  case object Red extends Colour
  case object Green extends Colour
  case object Blue extends Colour
}

case class Attempt(colourCounts: Map[Colour, Int]) {
  import Colour._
  import Math.max

  private def countFor(colour: Colour) = colourCounts.get(colour).getOrElse(0)

  def isPossible: Boolean =
    countFor(Red) <= 12 && 
    countFor(Green) <= 13 && 
    countFor(Blue) <= 14

  def combineMaximally(that: Attempt): Attempt = {
    val colours = colourCounts.keySet union that.colourCounts.keySet
    Attempt(colours.map(c => c -> max(countFor(c), that.countFor(c))).toMap)
  }

  def power: Int = countFor(Red) * countFor(Green) * countFor(Blue)
}

object Attempt {
  import Colour._

  private def parseColour(str: String): Option[(Colour, Int)] = {
    str.trim().split(" ").toList match
      case count :: "red" :: Nil => count.toIntOption.map(Red -> _)
      case count :: "green" :: Nil => count.toIntOption.map(Green -> _)
      case count :: "blue" :: Nil => count.toIntOption.map(Blue -> _)
      case _ => None
  }

  def parse(str: String): Attempt = 
    Attempt(str.split(",").map(parseColour).flatten.toMap)

  def parseMany(str: String): List[Attempt] = str.split(";").map(parse).toList
}

case class Game(id: Int, attempts: List[Attempt]) {
  def isPossible: Boolean = 
    attempts.forall(_.isPossible)

  def minimumPower: Int = attempts.reduce(_ combineMaximally _).power
}

object Game {
  def parseGameId(str: String): Int = 
    str.stripPrefix("Game ").toIntOption.getOrElse(0)

  def parse(line: String): Option[Game] = {
    line.split(":").toList match
      case gameId :: attemptData :: Nil => Some(Game(parseGameId(gameId), Attempt.parseMany(attemptData)))
      case _ => None
  }
}
object Day2 {
  import Colour._

  def getSumOfPowerOfAllGames(lines: List[String]): Int = 
    lines.flatMap(Game.parse).map(_.minimumPower).sum

  def getSum(lines: List[String]): Int = {
    lines
        .flatMap(Game.parse)
        .filter(_.isPossible)
        .map(_.id)
        .sum
  }
}

