package day6

import scala.util.parsing.combinator._

final case class Race(time: Long, record: Long) {
    private def findDiscriminant(a:Double, b:Double, c:Double) = Math.pow(b, 2) - (4d * a * c)

    private def findQuadraticRoots(a:Double, b:Double, c:Double) : Option[(Double, Double)] = {
        val discriminant = findDiscriminant(a, b, c)
        if (discriminant <= 0d) {
            None
        } else {
            List(1d, -1d)
                .map(_ * Math.sqrt(discriminant))
                .map(_ - b)
                .map(_ / (2d * a)) match {
                    case x :: y :: Nil => Some((x, y))
                    case _ => None
                }
        }
    }

    def getRaceButtonBounds(): Option[(Int, Int)] = {
        findQuadraticRoots(-1d, time.toDouble, -record.toDouble).flatMap{case (r1, r2) =>
            val x = if (r1.ceil == r1) {
                r1.ceil.toInt +1
            } else {
                r1.ceil.toInt
            } 

            val y = if (r2.floor == r2) {
                r2.floor.toInt -1
            } else {
                r2.floor.toInt
            }

            if (x > y) {
                None
            } else {
                Some((x, y))
            }
        }
    }

    def thereIsAnAnswer(): Boolean = getRaceButtonBounds().isDefined

    def distanceTravelled(buttonTime: Int) = buttonTime * (time - buttonTime)

    def winCount:Int = getRaceButtonBounds() match
        case None => 0
        case Some((lowerBound, upperBound)) => (upperBound - lowerBound) + 1
    
}

object RaceList {
    class Part1RaceParser extends RegexParsers {
        def number: Parser[Long] = """\d+""".r ^^ {_.toInt}
        def listOfNumbers: Parser[List[Long]] = number.+

        def raceList: Parser[List[Race]] = "Time:" ~ listOfNumbers ~ "Distance:" ~ listOfNumbers ^^ {case _ ~ times ~ _ ~ records => times.zip(records).map(Race.apply.tupled)}
    }

    class Part2RaceParser extends RegexParsers {
        def numeric: Parser[String] = """\d+""".r
        def bigNumber: Parser[Long] = numeric.+ ^^ {strs => strs.reduce(_ + _).toLong}

        def race: Parser[Race] = "Time:" ~ bigNumber ~ "Distance:" ~ bigNumber ^^ {case _ ~ time ~ _ ~ record => Race(time, record)}
    }


    def parseFromString(str: String): List[Race] = {
        val p = new Part1RaceParser()
        val r = p.parse(p.raceList, str)
        if (!r.successful) {
            Nil
        } else {
            r.get
        }
    }

    def parsePart2FromString(str: String): Race = {
        val p = new Part2RaceParser()
        val r = p.parse(p.race, str)
        r.get
    }

    def productOfWins(str: String): Int = parseFromString(str).map(_.winCount).reduce(_ * _)
    def winCountForSingleRace(str: String): Int = parsePart2FromString(str).winCount
}
