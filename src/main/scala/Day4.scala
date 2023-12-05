import scala.math.pow
import scala.annotation.tailrec

case class Card(winningNumbers: Set[String], numbers: Set[String]) {
    def score: Int = pow(2, winCount-1).toInt
    def winCount = (winningNumbers intersect numbers).size
}

object Card {
    def parse(line: String): Card = {
        val numbers = line.dropWhile(_ != ':').drop(1).split('|').toList
        val a: Set[String] = numbers(0).stripTrailing().grouped(3).toSet
        val b: Set[String] = numbers(1).stripTrailing().grouped(3).toSet
        Card(a, b)
    }
}

object Day4 {
  @tailrec
  def proliferate(cards: List[Card], idx: Int, copyCount: Map[Int, Int]): Int = {
    cards match
        case Nil => copyCount.values.sum
        case head :: tail => {
            val copiesOfCurrentCard = copyCount.getOrElse(idx, 0)
            val newCopyCounts = (1 to head.winCount).map(offset => {
                val currentCopiesOfCardAtOffset = copyCount.getOrElse(idx + offset, 0)
                (idx + offset, copiesOfCurrentCard+1 + currentCopiesOfCardAtOffset)
            }).toMap
            proliferate(
                tail,
                idx + 1,
                copyCount ++ newCopyCounts
            )
        }   
  }

  def totalScore(input: List[String]) = input.map(Card.parse).map(_.score).sum
  def proliferationCount(input: List[String]) = {
    val cards = input.map(Card.parse)
    val extras = proliferate(cards, 0, Map.empty)
    cards.size + extras
  }
}
