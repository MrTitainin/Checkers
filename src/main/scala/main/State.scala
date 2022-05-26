package main

import main.Game._
import _root_.main.State.Board
import _root_.main.UI._

import scala.swing.{Graphics2D, Point}
import scala.util.chaining.scalaUtilChainingOps

case class State(board:Board, whiteMove:Boolean, idle:Int) {

  val checkers:Seq[Checker] = board.filter(_.hasChecker).map(_.checker.get)
  def whites = checkers.filter(_.isWhite)
  def blacks = checkers.filter(_.isBlack)

  lazy val moves = (if(whiteMove) whites else blacks)
      .map(Move(_)(this)).foldLeft(List[Move]())((a,b)=>a:::b) // gets all possible moves for all checkers
      .pipe(ms=>ms.filter(_.hits == ms.maxBy(_.hits).hits)) // picks the ones with highest hit scores

  def movesFor(selected: Option[Checker]): List[Move] = selected match {
    case Some(checker) => moves.filter(_.origin == checker)
    case None => List()
  }

  var value = 0 // TODO calculate

  def draw(g:Graphics2D): Unit = board.foreach(_.draw(g))
  def lost = moves.isEmpty
  def isDraw = idle >= idleLimit

  def active = !lost && !isDraw

  def halfState = false

  def moved(from:Int, to:Int, queen:Boolean) = State(Move.moveFinal(from,to,board), !whiteMove, if(queen) idle+1 else idle)
  def hit(board:Board) = State(board, !whiteMove, 0)

  def halfHit(current:Int, board:Board) = {
    val newState = new State(board,whiteMove,idle){
      override lazy val moves = Move(board(current).checker.get)(this).filter(_.hits>0) // only hits for active checker
        .pipe(ms=>ms.filter(_.hits == ms.maxBy(_.hits).hits)) // picks the ones with highest hit scores
      override def halfState = true
    }
    if(newState.moves.isEmpty) {
      println("Warning: unnecessary half-hit")
      hit(board)
    }
    else newState
  }
}

object State {
  type Board = Seq[Tile]

  def x(i:Int):Int = i%boardLength
  def y(i:Int):Int = i/boardLength
  def pos(x:Int,y:Int) = y*boardLength+x

  def getTile(p:Point) = pos((p.x-left)/tileSize,(p.y-top)/tileSize)
  def tileStart(i:Int) = new Point(left+x(i)*tileSize,top+y(i)*tileSize)

  val startingState:State = {
    val whiteStartingPos = List(40,42,44,46,49,51,53,55,56,58,60,62)
    val blackStartingPos = List(1,3,5,7,8,10,12,14,17,19,21,23)
    def checker(i:Int) =
      if(whiteStartingPos.contains(i)) Checker(i,false,Checker.WHITE).some
      else if (blackStartingPos.contains(i)) Checker(i,false,Checker.BLACK).some else None
    val board = Seq.range(0,boardSize).map(i=>Tile(i,checker(i)))
    State(board,true,0)
  }

  def evaluate(s:State) = {

  }
}
