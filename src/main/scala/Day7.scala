package day7

import scala.util.parsing.combinator._

sealed trait Card {
    def score: Long
}

object Card {
    val base: Long = 14L

    case object Ace extends Card {val score = base -1}
    case object King extends Card {val score = base -2}
    case object Queen extends Card {val score = base -3}
    case object Jack extends Card {val score = base -4}
    case object Ten extends Card {val score = base -5}
    case object Nine extends Card {val score = base -6}
    case object Eight extends Card {val score = base -7}
    case object Seven extends Card {val score = base - 8}
    case object Six extends Card {val score = base -9}
    case object Five extends Card {val score = base - 10}
    case object Four extends Card {val score = base - 11}
    case object Three extends Card {val score = base -12}
    case object Two extends Card {val score = base -13}
    case object Joker extends Card {val score = base -14}
}

case class Hand(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card, bid: Long) {
    lazy val handTypeScore: Long = {
        val groups = List(c1, c2, c3, c4, c5).groupBy(identity)
        val jokerCount = groups.get(Card.Joker).fold(0)(_.size)

        (jokerCount, groups.size) match {
            case (5, _) => 6L // five of a kind
            case (js, 2) if js > 0 => 6L // five of a kind
            case (3, _) => 5L //four of a kind
            case (2, 3) => 5L // four of a kind
            case (1, 3) if groups.exists(_._2.size == 3) => 5L // 4 of a kind 
            case (1, 3) => 4L // or full house
            case (2, 4) => 3L // 3 of a kind
            case (1, 4) => 3L // 3 of a kind
            case (1, 5) => 1L // 1 pair
            case (0, 1) => 6L // five of a kind 
            case (0, 2) if groups.exists(_._2.size == 4) => 5L // four 
            case (0, 2) => 4L //or full house
            case (0, 3) if groups.exists(_._2.size == 3) => 3L //three 
            case (0, 3) => 2L // or two pair
            case (0, 4) => 1L  // pair
            case (0, 5) => 0L //0L // all different
        }
    }

    import Card.base

    lazy val score : Long=
        handTypeScore   * base * base * base * base * base + 
        c1.score        * base * base * base * base +
        c2.score        * base * base * base +
        c3.score        * base * base +
        c4.score        * base +
        c5.score
        
}

class Day7Parser extends RegexParsers {
     
    val ace: Parser[Card.Ace.type] = "A" ^^ {_ => Card.Ace}
    val king: Parser[Card.King.type] = "K" ^^ {_ => Card.King} 
    val queen: Parser[Card.Queen.type] = "Q" ^^ {_ => Card.Queen} 
    val jack: Parser[Card.Jack.type] = "J" ^^ {_ => Card.Jack} 
    val ten: Parser[Card.Ten.type] = "T" ^^ {_ => Card.Ten} 
    val nine: Parser[Card.Nine.type] = "9" ^^ {_ => Card.Nine} 
    val eight: Parser[Card.Eight.type] = "8" ^^ {_ => Card.Eight} 
    val seven: Parser[Card.Seven.type] = "7" ^^ {_ => Card.Seven} 
    val six: Parser[Card.Six.type] = "6" ^^ {_ => Card.Six} 
    val five: Parser[Card.Five.type] = "5" ^^ {_ => Card.Five} 
    val four: Parser[Card.Four.type] = "4" ^^ {_ => Card.Four} 
    val three: Parser[Card.Three.type] = "3" ^^ {_ => Card.Three} 
    val two: Parser[Card.Two.type] = "2" ^^ {_ => Card.Two}
    val joker: Parser[Card.Joker.type] = "J" ^^ {_ => Card.Joker}

    def card(j: Parser[Card]): Parser[Card] = ace | king | queen | j | ten | nine | eight | seven | six | five | four | three | two
    val number: Parser[Long] = """\d+""".r ^^ {_.toLong}


    def hand(j: Parser[Card]): Parser[Hand] = card(j) ~ card(j) ~ card(j) ~ card(j) ~ card(j) ~ number ^^ {case c1 ~ c2 ~ c3 ~ c4 ~ c5 ~ bid => Hand(c1, c2, c3, c4, c5, bid)}

    def listOfHands(j: Parser[Card]): Parser[List[Hand]] = hand(j).+
}

def parseFromString(input: String): List[Hand] = {
    val p = new Day7Parser
    val r = p.parse(p.listOfHands(p.jack), input) 
    if (!r.successful) {
        println(r)
    }
    r.get
}

def parseFromStringWithJokers(input: String): List[Hand] = {
    val p = new Day7Parser
    val r = p.parse(p.listOfHands(p.joker), input)
    if (!r.successful) {
        println(r)
    }
    r.get
}

def totalScore(hands: List[Hand]) = hands.sortBy(_.score).map(_.bid).zipWithIndex.map{case (bid, idx) => bid * (idx+1)}.sum

def getTotalScoreFromAllHands(input: String): Long = totalScore(parseFromString(input))
def getTotalScoreFromAllHandsWithJokers(input: String): Long = totalScore(parseFromStringWithJokers(input))
