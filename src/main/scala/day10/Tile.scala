package day10

import scala.annotation.tailrec
import day10.Vector2.unitFromDirectionChar
import cats.data.State

trait Tile {
    val northConnected: Boolean
    val southConnected: Boolean
    val eastConnected: Boolean
    val westConnected: Boolean
}

object Tile {
    def apply(c: Char): Tile = c match {
        case 'S' => StartTile
        case '.' => Ground
        case x => PipePiece(x)
    }
}

case class PipePiece(character: Char) extends Tile {
    val nc = List('|', 'J', 'L')
    val sc = List('|', '7', 'F')
    val ec = List('-', 'F', 'L')
    val wc = List('-', 'J', '7')

    val northConnected: Boolean = nc.contains(character)
    val southConnected: Boolean = sc.contains(character)
    val eastConnected: Boolean = ec.contains(character)
    val westConnected: Boolean = wc.contains(character)

    private lazy val connections: List[Char] = List(
        'N' -> northConnected,
        'S' -> southConnected,
        'E' -> eastConnected,
        'W' -> westConnected
    ).filter(_._2).map(_._1)

    def exit(entranceFrom: Char): Char = connections.filterNot(_ == entranceFrom).head

    private def mappedToBoxDrawing: Char = character match {
        case '-' => '\u2550'
        case 'F' => '\u2554'
        case 'L' => '\u255A'
        case 'J' => '\u255D'
        case '7' => '\u2557'
        case '|' => '\u2551'
        case x => x
    }
    

    override def toString(): String = mappedToBoxDrawing.toString()
}

object PipePiece {
    val horz = PipePiece('-')
    val vert = PipePiece('|')
}

trait NonPipeTile extends Tile {
    val northConnected: Boolean = false
    val southConnected: Boolean = false
    val eastConnected: Boolean = false
    val westConnected: Boolean = false
}

case object Ground extends NonPipeTile {
    override def toString() = "."
}
case object StartTile extends NonPipeTile {
    override def toString(): String = "S"
}