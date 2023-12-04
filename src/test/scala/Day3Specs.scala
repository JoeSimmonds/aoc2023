class Day3Specs extends munit.FunSuite {
    test("A schematic without any numbers is zero") {
        val input = List(
            "..........",
            "..........",
            "..........",
            ".........."
        ) 

        assertEquals(Day3.sumOfPartNumnbers(input), 0)
    }

    test("A number with no symbol is ignored") {
        val input = List(
            "..........",
            "..........",
            "....456...",
            ".........."
        ) 

        assertEquals(Day3.sumOfPartNumnbers(input), 0)
    }

    test("A number with a symbol after it is a part number") {
        val input = List(
            "..........",
            "..........",
            "....456*..",
            ".........."
        ) 

        assertEquals(Day3.sumOfPartNumnbers(input), 456)
    }

     test("A number with a symbol before it is a part number") {
        val input = List(
            "..........",
            "..........",
            "....^45...",
            ".........."
        ) 

        assertEquals(Day3.sumOfPartNumnbers(input), 45)
    }

    test("A number with a symbol above it is a part number") {
        val input = List(
            "..........",
            ".....{....",
            "....245...",
            ".........."
        ) 

        assertEquals(Day3.sumOfPartNumnbers(input), 245)
    }

    test("A number with a symbol above it is a part number") {
        val input = List(
            "...........",
            "...........",
            "....224....",
            ".....&....."
        ) 

        assertEquals(Day3.sumOfPartNumnbers(input), 224)
    }

        test("A number with a symbol diagonally near is a part Number") {
        val input = List(
            "...........",
            "...........",
            "....598....",
            ".......£..."
        ) 

        assertEquals(Day3.sumOfPartNumnbers(input), 598)
    }


    test("returns correct summ for example input") {
        val input = List(
            "467..114..",
            "...*......",
            "..35..633.",
            "......#...",
            "617*......",
            ".....+.58.",
            "..592.....",
            "......755.",
            "...$.*....",
            ".664.598.."
        )
        assertEquals(Day3.sumOfPartNumnbers(input), 4361)
    }

    test("doesn't miss out numbers at the end of the line") {
        val input = List(
            "...........",
            "...........",
            ".........23",
            ".........$."
        ) 
        assertEquals(Day3.sumOfPartNumnbers(input), 23)
    }

    test("Gets gear ratio of provided example") {
        val input = List(
            "467..114..",
            "...*......",
            "..35..633.",
            "......#...",
            "617*......",
            ".....+.58.",
            "..592.....",
            "......755.",
            "...$.*....",
            ".664.598.."
        )

        assertEquals(Day3.totalGearRatio(input), 467835)
    }
}
