package day10

import cats.data.State

case class Counts(insideCount: Int, outsideCount: Int, pipeCount: Int) {
    def incPipe = copy(pipeCount = this.pipeCount+1)
    def incOutside = copy(outsideCount = this.outsideCount+1)
    def incInside = copy(insideCount = this.insideCount+1)
}

sealed trait CounterState {
    def step(tile: Tile, currentCount: Counts): (CounterState, Counts)
}

object CounterState {
    case object Outside extends CounterState {
        def step(tile: Tile, currentCount: Counts): (CounterState, Counts) = tile match {
            case PipePiece('|') => (CrossingIn, currentCount.incPipe)
            case PipePiece('L') => (OnLowerBoundary, currentCount.incPipe)
            case PipePiece('F') => (OnUpperBoundary, currentCount.incPipe)
            case _ => (Outside, currentCount.incOutside)
        }

        override def toString(): String = "O"
    }

    case object Inside extends CounterState {
        def step(tile: Tile, currentCount: Counts): (CounterState, Counts) = tile match {
            case PipePiece('|') => (CrossingOut, currentCount.incPipe)
            case PipePiece('L') => (OnUpperBoundary, currentCount.incPipe)
            case PipePiece('F') => (OnLowerBoundary, currentCount.incPipe)
            case _ => (Inside, currentCount.incInside)
        }

        override def toString(): String = "I"
    }

    case object CrossingIn extends CounterState {
        def step(tile: Tile, currentCount: Counts): (CounterState, Counts) = tile match {
            case PipePiece('|') => (CrossingOut, currentCount.incPipe)
            case PipePiece('L') => (OnUpperBoundary, currentCount.incPipe)
            case PipePiece('F') => (OnLowerBoundary, currentCount.incPipe)
            case _ => (Inside, currentCount.incInside)
        }

        override def toString(): String = "<"
    }

    case object CrossingOut extends CounterState {
        def step(tile: Tile, currentCount: Counts): (CounterState, Counts) = tile match {
            case PipePiece('|') => (CrossingIn, currentCount.incPipe)
            case PipePiece('L') => (OnLowerBoundary, currentCount.incPipe)
            case PipePiece('F') => (OnUpperBoundary, currentCount.incPipe)
            case _ => (Outside, currentCount.incOutside)
        }

        override def toString(): String = ">"
    }

    case object OnLowerBoundary extends CounterState {
        def step(tile: Tile, currentCount: Counts): (CounterState, Counts) = tile match {
            case PipePiece('-') => (OnLowerBoundary, currentCount.incPipe)
            case PipePiece('7') => (CrossingIn, currentCount.incPipe)
            case PipePiece('J') => (CrossingOut, currentCount.incPipe)
        }

        override def toString(): String = "v"
    }
    
    case object OnUpperBoundary extends CounterState {
        def step(tile: Tile, currentCount: Counts): (CounterState, Counts) = tile match {
            case PipePiece('-') => (OnUpperBoundary, currentCount.incPipe)
            case PipePiece('7') => (CrossingOut, currentCount.incPipe) //
            case PipePiece('J') => (CrossingIn, currentCount.incPipe) //
        }

        override def toString(): String = "^"
    }

}
