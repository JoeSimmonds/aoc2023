package day10

import scala.annotation.tailrec
import utils.Vector2
import cats.data._
import cats.syntax.all._
import cats._
import org.w3c.dom.css.Counter
import utils.Vector2

case class PipeMap(input: List[String]) {
    def at(pos: Vector2): Tile = (for {
        line <- input.lift(pos.y)
        c <- line.lift(pos.x)
    } yield Tile(c)).getOrElse(Ground)

    def getTileInDirection(pos: Vector2, direction: Char): Tile = at(pos.move1(direction))

    lazy val startPosition : Vector2 = 
        input.map(_.indexOf('S')).zipWithIndex.collect {case (x, y) if x>=0 => Vector2(x, y)}.head


    lazy val firstStep: StepDef = {
        val n = getTileInDirection(startPosition, 'N')
        val s = getTileInDirection(startPosition, 'S')
        val e = getTileInDirection(startPosition, 'E')
        val w = getTileInDirection(startPosition, 'W')

        (n, s, e, w) match {
            case (PipePiece('|'), _, _, _) => StepDef(startPosition.move1('N'), 'N', PipePiece('|'), false)
            case (PipePiece('7'), _, _, _) => StepDef(startPosition.move1('N'), 'W', PipePiece('7'), false)
            case (PipePiece('F'), _, _, _) => StepDef(startPosition.move1('N'), 'E', PipePiece('F'), false)
            case (_, PipePiece('|'), _, _) => StepDef(startPosition.move1('S'), 'S', PipePiece('|'), false)
            case (_, PipePiece('L'), _, _) => StepDef(startPosition.move1('S'), 'E', PipePiece('L'), false)
            case (_, PipePiece('J'), _, _) => StepDef(startPosition.move1('S'), 'W', PipePiece('J'), false)
            case (_, _, PipePiece('-'), _) => StepDef(startPosition.move1('E'), 'E', PipePiece('-'), false)
            case (_, _, PipePiece('J'), _) => StepDef(startPosition.move1('E'), 'N', PipePiece('J'), false)
            case (_, _, PipePiece('7'), _) => StepDef(startPosition.move1('E'), 'S', PipePiece('7'), false)
            case (_, _, _, PipePiece('-')) => StepDef(startPosition.move1('W'), 'W', PipePiece('-'), false)
            case (_, _, _, PipePiece('L')) => StepDef(startPosition.move1('W'), 'N', PipePiece('L'), false)
            case (_, _, _, PipePiece('F')) => StepDef(startPosition.move1('W'), 'S', PipePiece('F'), false)
        }
    }

    private lazy val startPieceType: PipePiece = {
        val n = getTileInDirection(startPosition, 'N').southConnected
        val s = getTileInDirection(startPosition, 'S').northConnected
        val e = getTileInDirection(startPosition, 'E').westConnected
        val w = getTileInDirection(startPosition, 'W').eastConnected

        (n, s, e, w) match {
            case (true, true, _, _) => PipePiece('|')
            case (_, _, true, true) => PipePiece('-')
            case (true, _, _, true) => PipePiece('J')
            case (_, true, true, _) => PipePiece('F')
            case (true, _, true, _) => PipePiece('L')
            case (_, true, _, true) => PipePiece('7')
        }
    }

    private def OppositeDir(dir: Char) = dir match {
        case 'N' => 'S'
        case 'S' => 'N'
        case 'E' => 'W'
        case 'W' => 'E'
    }

    @tailrec
    private def loop(currentSteps: List[StepDef]): List[StepDef] = {
        val latest = currentSteps.head
        val nextStepTile = getTileInDirection(latest.position, latest.exitDir)
        val nextPosition = latest.position.move1(latest.exitDir)

        (latest.exitDir, nextStepTile) match {
            case (_, StartTile) => StepDef(latest.position.move1(latest.exitDir), startPieceType.exit(OppositeDir(latest.exitDir)), startPieceType, true) +: currentSteps

            case ('N', p@PipePiece('|')) => loop(StepDef(latest.position.move1(latest.exitDir), 'N', p, false) +: currentSteps)
            case ('N', p@PipePiece('F')) => loop(StepDef(latest.position.move1(latest.exitDir), 'E', p, false) +: currentSteps)
            case ('N', p@PipePiece('7')) => loop(StepDef(latest.position.move1(latest.exitDir), 'W', p, false) +: currentSteps)
            case ('S', p@PipePiece('|')) => loop(StepDef(latest.position.move1(latest.exitDir), 'S', p, false) +: currentSteps)
            case ('S', p@PipePiece('J')) => loop(StepDef(latest.position.move1(latest.exitDir), 'W', p, false) +: currentSteps)
            case ('S', p@PipePiece('L')) => loop(StepDef(latest.position.move1(latest.exitDir), 'E', p, false) +: currentSteps)
            case ('E', p@PipePiece('-')) => loop(StepDef(latest.position.move1(latest.exitDir), 'E', p, false) +: currentSteps)
            case ('E', p@PipePiece('7')) => loop(StepDef(latest.position.move1(latest.exitDir), 'S', p, false) +: currentSteps)
            case ('E', p@PipePiece('J')) => loop(StepDef(latest.position.move1(latest.exitDir), 'N', p, false) +: currentSteps)
            case ('W', p@PipePiece('-')) => loop(StepDef(latest.position.move1(latest.exitDir), 'W', p, false) +: currentSteps)
            case ('W', p@PipePiece('F')) => loop(StepDef(latest.position.move1(latest.exitDir), 'S', p, false) +: currentSteps)
            case ('W', p@PipePiece('L')) => loop(StepDef(latest.position.move1(latest.exitDir), 'N', p, false) +: currentSteps)
        }
    }

    def identifyLoop():List[Vector2] = loop(List(firstStep)).map(_.position)

    def furthestDistanceInLoop() = identifyLoop().size/2

    def cleaned(): List[List[Tile]] = {
        val loopPositions = identifyLoop()
        (for {
            (line, lineNo) <- input.zipWithIndex
            (character, characterIndex) <- line.zipWithIndex
        } yield if (loopPositions.contains(Vector2(characterIndex, lineNo))) {
            if (character == 'S') {
                startPieceType
            } else{
                PipePiece(character)
            }    
        } else {
            Ground
        }).grouped(input.head.size).toList
    }

    def tileInsideOneLine(line: List[Tile]): Int = {
        val result = line.foldLeft[(CounterState, Counts)]((CounterState.Outside, Counts(0,0,0))){case ((s, cur), t) => 
            val res = s.step(t, cur)
            if (res._1 == CounterState.Outside || res._1 == CounterState.Inside) {
                print(res._1)
            } else {
                print(t)
            }
            res
        }._2
        println(s" => $result")
        result.insideCount
    }

    def tilesInside(): Int = {
        val c = cleaned()
        println(c.map(_.mkString).mkString("\r\n"))
        println()
        c.map(tileInsideOneLine).sum
    }
}