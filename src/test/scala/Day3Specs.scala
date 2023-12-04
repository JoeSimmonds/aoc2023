class Day3Specs extends munit.FunSuite {
    test("A schematic without any numbers is zero") {
        val input = List(
            "..........",
            "..........",
            "..........",
            ".........."
        ) 

        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 0)
    }

    test("A number with no symbol is ignored") {
        val input = List(
            "..........",
            "..........",
            "....456...",
            ".........."
        ) 

        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 0)
    }

    test("A number with a symbol after it is a part number") {
        val input = List(
            "..........",
            "..........",
            "....456*..",
            ".........."
        ) 

        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 456)
    }

     test("A number with a symbol before it is a part number") {
        val input = List(
            "..........",
            "..........",
            "....^45...",
            ".........."
        ) 

        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 45)
    }

    test("A number with a symbol above it is a part number") {
        val input = List(
            "..........",
            ".....{....",
            "....245...",
            ".........."
        ) 

        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 245)
    }

    test("A number with a symbol above it is a part number") {
        val input = List(
            "...........",
            "...........",
            "....224....",
            ".....&....."
        ) 

        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 224)
    }

        test("A number with a symbol diagonally near is a part Number") {
        val input = List(
            "...........",
            "...........",
            "....598....",
            ".......£..."
        ) 

        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 598)
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
        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 4361)
    }

    test("doesn't miss out numbers at the end of the line") {
        val input = List(
            "...........",
            "...........",
            ".........23",
            ".........$."
        ) 
        assertEquals(new day3.Schematic(input).sumOfPartNumbers(), 23)
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

        assertEquals(new day3.Schematic(input).totalGearRatio(), 467835)
    }
}
