package day10

import scala.annotation.tailrec
import cats.data.State
import utils.Vector2

case class StepDef(position: Vector2, exitDir:Char, pipePiece: PipePiece, isStart: Boolean ){
    val x = position._1
    val y = position._2
}