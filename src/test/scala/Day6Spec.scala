import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import org.scalacheck.Gen
import day6._

class Day6Spec extends ScalaCheckSuite {
  override val scalaCheckInitialSeed = "lrH4L-ASmYzXb48b-D9cnxqcupFLF2o7wLnLWB4yC4A="

  val raceDefinitionGen = (for {
    time <- Gen.posNum[Int]
    record <- Gen.posNum[Int] 
  } yield Race(time, record)) suchThat (_.thereIsAnAnswer())

    property("If there is an answer then both answers beat the record and widening the range fails") {
        forAllNoShrink(raceDefinitionGen){race => {
            val (n1, n2) = race.getRaceButtonBounds().get
            assert(clue(race.distanceTravelled(n1)) > clue(race.record))
            assert(race.distanceTravelled(n2) > clue(race.record))
            assert(race.distanceTravelled(n1-1) <= clue(race.record))
            assert(race.distanceTravelled(n2+1) <= clue(race.record)) 
        }}
    }

    test("provided example") {
        val input = """
        |Time:      7  15   30
        |Distance:  9  40  200
        |""".stripMargin

        assertEquals(RaceList.productOfWins(input), 288)
    }

    test("An unbeatabble record without sub-millisecond accuracy" ) {
        val SUT = Race(7, 12)
        assertEquals(SUT.getRaceButtonBounds(), None)

    }

    test("part2 Provided Example") {
        val input = """
            |Time:      7  15   30
            |Distance:  9  40  200
            |""".stripMargin

        assertEquals(RaceList.winCountForSingleRace(input), 71503)
    }
}
