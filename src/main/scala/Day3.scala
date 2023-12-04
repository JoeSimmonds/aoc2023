import scala.annotation.tailrec

final case class SymbolPosition(symbol: Char, horz: Int, lineNo: Int) {
    def gearRatio(numberPositions: List[NumberPosition]): Option[Int] = {
        if (symbol != '*') {
            None
        } else {
            val nums = numberPositions.filter(np => np.isNextTo(this))
            if (nums.size != 2) {
                None
            } else {
                Some(nums.map(_.number).reduce(_ * _))
            }
        }
    }
}

final case class NumberPosition(number: Int, left: Int, right: Int, lineNo: Int) {
    def isNextTo(sp: SymbolPosition): Boolean = 
        sp.lineNo >= lineNo -1 && 
        sp.lineNo <= lineNo +1 && 
        sp.horz >= left -1 && 
        sp.horz <= right +1

    def isPartNo(symbolPositions: List[SymbolPosition]): Boolean = symbolPositions.exists(isNextTo)
}



object Day3 {

  @tailrec
  private def scanLineForNumberRec(line: String, currentNumber:String, currentPosition:Int, acc: List[NumberPosition], lineNo: Int): List[NumberPosition] = {
    line.headOption match
        case None if currentNumber == "" => acc
        case None => acc :+ NumberPosition(
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
                currentPosition+1,
                acc,
                lineNo
                )
        case _ =>
            scanLineForNumberRec(
                line.tail,
                "",
                currentPosition+1,
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
  private def scanLineForSymbolRec(line: String, lineNo: Int, currentPosition: Int, acc:List[SymbolPosition]) : List[SymbolPosition] = {
    line.headOption match
        case None => acc
        case Some(next) if (next.isDigit || next == '.') =>
            scanLineForSymbolRec(line.tail, lineNo, currentPosition+1, acc)
        case Some(next) =>
            scanLineForSymbolRec(line.tail, lineNo, currentPosition+1, acc :+ SymbolPosition(next, currentPosition, lineNo))
  }

  private def scanLineForSymbol(line: String, lineNo: Int): List[SymbolPosition] = scanLineForSymbolRec(line, lineNo, 0, Nil)

  private def scanLineForNumber(line: String, lineNo: Int) = scanLineForNumberRec(line, "", 0, Nil, lineNo)

  def sumOfPartNumnbers(schematic: List[String]): Int = {
    val numberPositions: List[NumberPosition] = schematic.zipWithIndex.map{case (l, i) => scanLineForNumber(l, i)}.flatten
    val symbolPositions: List[SymbolPosition] = schematic.zipWithIndex.map{case (l, i) => scanLineForSymbol(l, i)}.flatten

    numberPositions.filter(_.isPartNo(symbolPositions)).map(_.number).sum
  }

  def totalGearRatio(schematic: List[String]): Int = {
    val numberPositions: List[NumberPosition] = schematic.zipWithIndex.map{case (l, i) => scanLineForNumber(l, i)}.flatten
    val symbolPositions: List[SymbolPosition] = schematic.zipWithIndex.map{case (l, i) => scanLineForSymbol(l, i)}.flatten
    
    symbolPositions.flatMap(_.gearRatio(numberPositions)).sum
  }
}
