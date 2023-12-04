package day3

final case class NumberPosition(
    number: Int,
    left: Int,
    right: Int,
    lineNo: Int
) {
  def isNextTo(sp: SymbolPosition): Boolean =
    sp.lineNo >= lineNo - 1 &&
      sp.lineNo <= lineNo + 1 &&
      sp.horz >= left - 1 &&
      sp.horz <= right + 1

  def isPartNo(symbolPositions: List[SymbolPosition]): Boolean =
    symbolPositions.exists(isNextTo)
}