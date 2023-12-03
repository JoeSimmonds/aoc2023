class Day2Specs extends munit.FunSuite {

/*
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
*/

// only 12 red cubes, 13 green cubes, and 14 blue cubes

    test("returns the Id of a single line with a single possible attempt") { 
        val input = "Game 376: 1 blue, 2 green"
        assertEquals(Day2.getSum(List(input)), 376)
    }

    test("Excludes the Id of a line with a single impossible attempt") {
        val input = "Game 34: 34 blue, 2 green"
        assertEquals(Day2.getSum(List(input)), 0)
    }

    test("Excludes a game with only one failing attempt") {
        val input = "Game 3: 2 green, 5 red; 34 red, 2 green; 5 blue, 2 green"
        assertEquals(Day2.getSum(List(input)), 0)
    }

    test("Sums up the Ids of possible games") {
        val input = List(
            "Game 3: 2 green, 5 red; 34 red, 2 green; 5 blue, 2 green",
            "Game 376: 1 blue, 2 green",
            "Game 43: 1 green, 3 blue, 6 red")
        assertEquals(Day2.getSum(input), 419)
    }

    test("gets the power right for a multi attempt game") {
        val input = "Game 376: 3 blue, 2 green; 7 red, 2 blue; 4 red, 11 green"
        assertEquals(Day2.getSumOfPowerOfAllGames(List(input)), 231)
    }

    test("Gets the power right for the provided example") {
        val input = List(
            "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        )

        assertEquals(Day2.getSumOfPowerOfAllGames(input), 2286)
    }
}
