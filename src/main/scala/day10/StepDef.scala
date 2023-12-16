package day10

import scala.annotation.tailrec
import day10.Vector2.unitFromDirectionChar
import cats.data.State

case class StepDef(position: Vector2, exitDir:Char, pipePiece: PipePiece, isStart: Boolean ){
    val x = position._1
    val y = position._2
}