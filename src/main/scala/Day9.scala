package day9

import scala.util.parsing.combinator.RegexParsers

object Day9Parser extends RegexParsers {
    override def skipWhitespace: Boolean = false
    val number: Parser[Long] = """-?\d+""".r ^^ {_.toLong}
    val spaces: Parser[String] = """ """.r
    val newline: Parser[String] = "\r\n" | "\r" | "\n"

    val line: Parser[List[Long]] = number ~ (spaces ~> number).* ^^ mkList
    val dataFile: Parser[List[List[Long]]] = line ~ (newline ~> line).* ^^ mkList
}

def diffs(series: List[Long]): List[Long] = series.sliding(2).map(w => w(1) - w(0)).toList

def projectNext(series: List[Long]): Long = {
    if (series.groupBy(identity).size == 1) {
        series(0)
    } else {
        series.last + projectNext(diffs(series))
    }
}

def projectPrevious(series: List[Long]): Long = {
        if (series.groupBy(identity).size == 1) {
        series(0)
    } else {
        series.head - projectPrevious(diffs(series))
    }
}

def sumOfNextValues(input: String): Long = {
    val result = Day9Parser.parse(Day9Parser.dataFile, input).get
    result.map(projectNext).sum
}

def sumOfPreviousValues(input: String): Long = {
    val result = Day9Parser.parse(Day9Parser.dataFile, input).get
    result.map(projectPrevious).sum
}
