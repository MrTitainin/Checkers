package algorithms

import algorithms.MinMax._
import main.{AIAlgorithm, State}

class MinMax(val maxDepth:Int=defaultDepth) extends AIAlgorithm {
  override def makeMove(s: State): State = {
    evaluate(s)
    (if(s.whiteMove) s.moves.maxBy(_.result.value) else s.moves.minBy(_.result.value)).result
  }
  def evaluate(s:State, depth:Int=maxDepth):Int = {
    s.value = if(s.lost){
        if(s.whiteMove) loseValue
        else winValue
      }
      else if(s.isDraw) drawValue
      else if(depth > 0){
        val children = s.moves.map(m => evaluate(m.result,depth-1))
        if(s.whiteMove) children.max else children.min
      }
    else simpleValue(s)
    s.value
  }

  override def avgWinMoves: Int = ???

  override def avgMoveTime: Long = ???

  override def winRate: Double = ???

  override def gameEnded(win: Boolean, moves: Int): Unit = ???
}
object MinMax{
  val drawValue = 0
  val winValue = Integer.MAX_VALUE
  val loseValue = Integer.MIN_VALUE
  val defaultDepth = 5

  def simpleValue(s:State) = s.whites.size + 4 * s.whites.count(_.isQueen) - s.blacks.size - 4 * s.blacks.count(_.isQueen)
}
