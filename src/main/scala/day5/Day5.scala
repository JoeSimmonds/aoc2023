package day5

import scala.collection.Iterable
import scala.util.parsing.combinator._


sealed trait RangeMap {
    def source: String
    def destination: String
    def apply(key: Long): Long

    def andThen(that: RangeMap): RangeMap = CombinedRangeMap(this, that)
}

object RangeMap {
    def apply(source: String, destination: String, ranges: List[MappedRange]) = SimpleRangeMap(source, destination, ranges)
}

case class SimpleRangeMap(source: String, destination: String, ranges: List[MappedRange]) extends RangeMap {
    
    def apply(key: Long): Long = ranges.find(_.appliesTo(key)).fold(key)(_.apply(key))
}

case class CombinedRangeMap(a:RangeMap, b:RangeMap) extends RangeMap {
  assert(a.destination == b.source)

  override def apply(key: Long): Long = b(a(key))

  override def destination: String = b.destination

  override def source: String = a.source
}

case class MappedRange(sourceStart: Long, destinationStart: Long, range: Long) {
    
    def appliesTo(key: Long):Boolean = key >= sourceStart && key < (sourceStart + range)

    def apply(key: Long): Long = if (appliesTo(key)) {
       destinationStart + (key - sourceStart)
    } else {
        key
    }
}

case class Almanac(seeds: List[SeedRange], map: RangeMap) {
    assert(map.source=="seed" && map.destination == "location")

    def isInAnySeedRange(mappedValue: Long): Boolean = seeds.exists((sr:SeedRange) => sr.isInRange(mappedValue))

    def getClosestLocation(): Option[Long] = {
        seeds.map(_.getRange()).reduce(_ ++ _).map(map.apply).toList.sorted.headOption
    }
}

case class ReversedAlmanac(seeds: List[SeedRange], map: RangeMap) {
    assert(map.source=="location" && map.destination == "seed")

    def isInAnySeedRange(mappedValue: Long): Boolean = seeds.exists((sr:SeedRange) => sr.isInRange(mappedValue))

    def getClosestLocation(maxCandidate: Long = Int.MaxValue.toLong): Option[Long] = {
        Stream.range(1L, maxCandidate).find(candidate => isInAnySeedRange(map(candidate)))
    }
}

case class SeedRange(start: Long, range: Long) {
    def isInRange(num: Long) = num >= start && num < (start+range)
    def getRange() = (start to (start+range-1)).toIterator
}

object Almanac {
    class CommonParser extends RegexParsers {
        def number: Parser[Long] = """\d+""".r ^^ {_.toLong}
        def listOfNumbers: Parser[List[Long]] = number.*

        def name: Parser[String] = """[a-z]+""".r
        
       
        
    }

    class Part1Parser extends CommonParser {
        def mappedRange: Parser[MappedRange] = number ~ number ~ number ^^ {case sourceStart ~ destStart ~ range => MappedRange(destStart, sourceStart, range)}
        def rangeMap: Parser[RangeMap] = name ~ "-to-" ~ name ~ "map:" ~ mappedRange.* ^^ {case a ~ _ ~ b ~ _ ~ ranges => RangeMap(a, b, ranges) }
        def mulltiRangeMap: Parser[RangeMap] = rangeMap.* ^^ {maps => maps.reduce(_ andThen _)}
        def almanac: Parser[Almanac] = "seeds:" ~ listOfNumbers ~ mulltiRangeMap ^^ {case _ ~ seeds ~ rangeMap => Almanac(seeds.map(SeedRange.apply(_, 1)), rangeMap)}
    }

    class Part2Parser extends CommonParser {
        def mappedRange: Parser[MappedRange] = number ~ number ~ number ^^ {case sourceStart ~ destStart ~ range => MappedRange(sourceStart, destStart, range)}
        def rangeMap: Parser[RangeMap] = name ~ "-to-" ~ name ~ "map:" ~ mappedRange.* ^^ {case a ~ _ ~ b ~ _ ~ ranges => RangeMap(b, a, ranges) }
        def mulltiRangeMap: Parser[RangeMap] = rangeMap.* ^^ {maps => maps.reverse.reduce(_ andThen _)}
        def seedRange: Parser[SeedRange] = number ~ number ^^ {case start ~ range => SeedRange(start, range)}
        def almanac: Parser[ReversedAlmanac] = "seeds:" ~ seedRange.* ~ mulltiRangeMap ^^ {case _ ~ seedRanges ~ rangeMap => ReversedAlmanac(seedRanges, rangeMap)}
    }

    def parseFromString(str: String) : Almanac = {
        val p = new Part1Parser()
        p.parse(p.almanac, str).get
    }

    def parseV2FromString(str: String) : ReversedAlmanac = {
        val p = new Part2Parser()
        p.parse(p.almanac, str).get
    }

}
