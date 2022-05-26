package algorithms

import algorithms.MinMax._
import main.{AIAlgorithm, Move, State}

import scala.annotation.tailrec

class AlphaBeta(val maxDepth:Int=defaultDepth) extends AIAlgorithm {
  override def makeMove(s: State): State = {
    evaluate(s)._2.result
  }
  def evaluate(s:State, depth:Int=maxDepth, ialpha:Int = Integer.MIN_VALUE, ibeta:Int = Integer.MAX_VALUE):(Int,Move) = {
    var best:Move = null
    s.value = if(s.lost){
      if(s.whiteMove) loseValue
      else winValue
    }
    else if(s.isDraw) drawValue
    else if(depth > 0) {
      if(s.whiteMove){
        var alpha = ialpha
        var mv = Integer.MIN_VALUE
        var cmax:Move = s.moves.head
        @tailrec def maximize(mvs:List[Move]=s.moves.toList):Move = if(mvs==Nil) cmax else{
          val v = evaluate(mvs.head.result,depth-1,alpha,ibeta)._1
          if(v>alpha){
            alpha = v
            cmax = mvs.head
          }
          if(v>=ibeta) cmax
          else maximize(mvs.tail)
        }
        best = maximize()
        best.result.value
      }
      else {
        var beta = ibeta
        var mv = Integer.MAX_VALUE
        var cmin:Move = s.moves.head
        @tailrec def minimize(mvs:List[Move]=s.moves.toList):Move = if(mvs==Nil) cmin else{
          val v = evaluate(mvs.head.result,depth-1,ialpha,beta)._1
          if(v<beta){
            beta = v
            cmin = mvs.head
          }
          if(v<=ialpha) cmin
          else minimize(mvs.tail)
        }
        best = minimize()
        best.result.value
      }
    }
    else{
      simpleValue(s)
    }
    (s.value,best)
  }

  override def avgWinMoves: Int = ???

  override def avgMoveTime: Long = ???

  override def winRate: Double = ???

  override def gameEnded(win: Boolean, moves: Int): Unit = ???
}

