package day3

import scala.annotation.tailrec

class Schematic(lines: List[String]) {

  private lazy val numberPositions: List[NumberPosition] = lines.zipWithIndex.map {
      case (l, i) => scanLineForNumber(l, i)
    }.flatten

  private lazy val symbolPositions: List[SymbolPosition] = lines.zipWithIndex.map {
      case (l, i) => scanLineForSymbol(l, i)
    }.flatten

  def sumOfPartNumbers(): Int = numberPositions.filter(_.isPartNo(symbolPositions)).map(_.number).sum
  def totalGearRatio(): Int = symbolPositions.flatMap(_.gearRatio(numberPositions)).sum

  @tailrec
  private def scanLineForNumberRec(
      line: String,
      currentNumber: String,
      currentPosition: Int,
      acc: List[NumberPosition],
      lineNo: Int
  ): List[NumberPosition] = {
    line.headOption match
      case None if currentNumber == "" => 
        acc
      case None =>
        acc :+ NumberPosition(
          currentNumber.toIntOption.getOrElse(0),
          currentPosition - currentNumber.size,
          currentPosition - 1,
          lineNo
        )
      case Some(next) if (next.isDigit) =>
        scanLineForNumberRec(
          line.tail,
          currentNumber :+ next,
          currentPosition + 1,
          acc,
          lineNo
        )
      case Some(next) if currentNumber == "" =>
        scanLineForNumberRec(
          line.tail,
          "",
          currentPosition + 1,
          acc,
          lineNo
        )
      case _ =>
        scanLineForNumberRec(
          line.tail,
          "",
          currentPosition + 1,
          acc :+ NumberPosition(
            currentNumber.toIntOption.getOrElse(0),
            currentPosition - currentNumber.size,
            currentPosition - 1,
            lineNo
          ),
          lineNo
        )
  }

  @tailrec
  private def scanLineForSymbolRec(
      line: String,
      lineNo: Int,
      currentPosition: Int,
      acc: List[SymbolPosition]
  ): List[SymbolPosition] = {
    line.headOption match
      case None => acc
      case Some(next) if (next.isDigit || next == '.') =>
        scanLineForSymbolRec(line.tail, lineNo, currentPosition + 1, acc)
      case Some(next) =>
        scanLineForSymbolRec(
          line.tail,
          lineNo,
          currentPosition + 1,
          acc :+ SymbolPosition(next, currentPosition, lineNo)
        )
  }

  private def scanLineForSymbol(line: String, lineNo: Int): List[SymbolPosition] = scanLineForSymbolRec(line, lineNo, 0, Nil)

  private def scanLineForNumber(line: String, lineNo: Int) = scanLineForNumberRec(line, "", 0, Nil, lineNo)
}