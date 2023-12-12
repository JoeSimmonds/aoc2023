import day7._
import day7.Card._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import scala.util.Try

class Day7Spec extends munit.ScalaCheckSuite {
    test("can parse hands with all cards") {
        val input = """
        |AKQJT 45
        |98765 6
        |432AK 984
        |""".stripMargin

        val result = day7.parseFromString(input)

        assertEquals(result, List(
            Hand(Ace, King, Queen, Jack, Ten, 45),
            Hand(Nine, Eight, Seven, Six, Five, 6),
            Hand(Four, Three, Two, Ace, King, 984)
        ))
    }

    test("A hand has a comparative score just for the cards") {
        val input1 = "23456 556"
        val input2 = "34567 12"

        val hand1 = day7.parseFromString(input1).head
        val hand2 = day7.parseFromString(input2).head

        assert(hand2.score > hand1.score) 
    }

    /*
    *
Five of a kind, where all five cards have the same label: AAAAA
Four of a kind, where four cards have the same label and one card has a different label: AA8AA
Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
High card, where all cards' labels are distinct: 23456
    */
  

    test("Hand score is more important") {
        val input1 = "22222 556" // five of a kind
        val input2 = "33334 12"  // four of a kind
        val input3 = "44455 67"  // full house
        val input4 = "55677 789" // two pair
        val input5 = "7899T 89"  //one pair
        val input6 = "9TJQK 77"  //High card

        val hand1 = day7.parseFromString(input1).head
        val hand2 = day7.parseFromString(input2).head
        val hand3 = day7.parseFromString(input3).head
        val hand4 = day7.parseFromString(input4).head
        val hand5 = day7.parseFromString(input5).head
        val hand6 = day7.parseFromString(input6).head

        assert(hand1.score > hand2.score) 
        assert(hand2.score > hand3.score) 
        assert(hand3.score > hand4.score) 
        assert(hand4.score > hand5.score) 
        assert(hand5.score > hand6.score) 
    }

    test("the provided example") {
        val input = """
        |32T3K 765
        |T55J5 684
        |KK677 28
        |KTJJT 220
        |QQQJA 483
        |""".stripMargin

        assertEquals(day7.getTotalScoreFromAllHands(input), 6440L)
    }

    val cardGenWithJoker: Gen[Card] = Gen.oneOf(Ace, King, Queen, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Joker)
    val handGenWithJokers: Gen[Hand] = for {
        c1 <- cardGenWithJoker
        c2 <- cardGenWithJoker
        c3 <- cardGenWithJoker
        c4 <- cardGenWithJoker
        c5 <- cardGenWithJoker
        b <- Gen.long
    } yield Hand(c1, c2, c3, c4, c5, b)

    property("All hands can be scored without error") {
        forAllNoShrink(handGenWithJokers) {hand =>
            assert(Try{hand.score}.isSuccess)
        }
    }

    test("the part 2 provided examples seperately") {
        val input1 = "32T3K 765"
        val input2 = "T55J5 684"
        val input3 = "KK677 28"
        val input4 = "KTJJT 220"
        val input5 = "QQQJA 483"

        assertEquals(day7.parseFromStringWithJokers(input1).head.handTypeScore, 1L)
        assertEquals(day7.parseFromStringWithJokers(input2).head.handTypeScore, 5L)
        assertEquals(day7.parseFromStringWithJokers(input3).head.handTypeScore, 2L)
        assertEquals(day7.parseFromStringWithJokers(input4).head.handTypeScore, 5L)
        assertEquals(day7.parseFromStringWithJokers(input5).head.handTypeScore, 5L)
    }

    test("the part 2 provided example") {
        val input = """
        |32T3K 765
        |T55J5 684
        |KK677 28
        |KTJJT 220
        |QQQJA 483
        |""".stripMargin

        assertEquals(day7.getTotalScoreFromAllHandsWithJokers(input), 5905L)
    }
}
 