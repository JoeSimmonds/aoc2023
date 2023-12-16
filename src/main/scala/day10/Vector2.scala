package day10

import scala.annotation.tailrec
import day10.Vector2.unitFromDirectionChar
import cats.data.State

case class Vector2(x: Int, y: Int) {
    def + (that: Vector2): Vector2 = Vector2(this.x + that.x, this.y + that.y)
    def - (that: Vector2): Vector2 = Vector2(this.x - that.x, this.y - that.y)
    def move1(dir: Char): Vector2 = this + unitFromDirectionChar(dir)
    def asTuple: (Int, Int) = (this.x, this.y)
}

object Vector2 {
    val north1 = Vector2(0, -1)
    val south1 = Vector2(0, 1)
    val east1 = Vector2(1, 0)
    val west1 = Vector2(-1, 0)

    def unitFromDirectionChar(dir: Char): Vector2 = dir match {
        case 'N' => north1
        case 'S' => south1
        case 'E' => east1
        case 'W' => west1     
    }
}