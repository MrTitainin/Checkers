package algorithms

import algorithms.MinMax._
import main.AIAlgorithm._
import main.{AIAlgorithm, EvaluateHeuristic, State}

case class MinMax(maxDepth:Int=defaultDepth, evaluate: EvaluateHeuristic = EvaluateHeuristic.simple) extends AIAlgorithm {

  override def makeMove(s: State): State = {
    val init = System.nanoTime()
    searchTree(s)
    val result = (if(s.whiteMove) s.moves.maxBy(_.result.value) else s.moves.minBy(_.result.value)).result
    val end = System.nanoTime()
    totalMoves+=1
    timeSum += end-init
    result
  }

  def searchTree(s:State, depth:Int=maxDepth):Int = {
    s.value = if(s.lost){
        if(s.whiteMove) loseValue
        else winValue
      }
      else if(s.isDraw) drawValue
      else if(depth > 0){
        val children = s.moves.map(m => searchTree(m.result,depth-1))
        if(s.whiteMove) children.max else children.min
      }
    else evaluate(s)
    s.value
  }
}
object MinMax {
  val defaultDepth = 5
}
