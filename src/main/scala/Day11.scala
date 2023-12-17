package day11

import utils.Vector2

def findExpandedLines(input: List[List[Char]]): List[Int] = input.zipWithIndex.filter(_._1.forall(_ == '.')).map(_._2)

def findGalaxyCoords(input: List[List[Char]], expandedLines: List[Int], expandedColumns: List[Int], expansionRate: Int): List[Vector2] = {
    val originalCoords = for {
        (l, y) <- input.zipWithIndex
        (c, x) <- l.zipWithIndex
         v <- if (c == '#') Some(Vector2(x, y)) else None
    } yield v


    originalCoords.map(v => Vector2(
            v.x + (expandedColumns.filter(_ < v.x).size * (expansionRate -1)),
            v.y + (expandedLines.filter(_ < v.y).size * (expansionRate -1))
        ))
}

def totalInterGalacticDistance(input: List[String], expansionRate: Int): Long = {
    val expandedLines = findExpandedLines(input.map(_.toList))
    val expandedColumns = findExpandedLines(input.map(_.toList).transpose)

    val coords = findGalaxyCoords(input.map(_.toList), expandedLines, expandedColumns, expansionRate)
    val allDistances = for {
        a <- coords
        b <- coords
    } yield {
        (a - b).rectangularDistance
    }

    println(allDistances.size)
    allDistances.map(_.toLong).sum / 2
}
