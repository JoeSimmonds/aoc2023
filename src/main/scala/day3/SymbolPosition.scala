package day3

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