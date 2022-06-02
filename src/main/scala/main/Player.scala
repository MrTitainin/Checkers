package main

sealed trait Player{
  def ai = this.asInstanceOf[AIAlgorithm]
  def isHuman = this == Human
  def isAI = this != Human
}

case object Human extends Player

abstract class AIAlgorithm extends Player {

  def makeMove(s:State):State

  var timeSum:Long = 0
  var totalMoves = 0

  var games = 0
  var wins = 0
  var winMoves = 0

  def avgWinMoves: Int = if(wins>0) winMoves/wins else 0

  def avgMoveTime: Long = timeSum/totalMoves

  def winRate: Double = wins.toDouble/games

  def gameEnded(win: Boolean, moves: Int): Unit = {
    games+=1
    if(win){
      wins+=1
      winMoves+=moves
    }
  }
}
object AIAlgorithm {

  val drawValue = 0
  val winValue = Integer.MAX_VALUE
  val loseValue = Integer.MIN_VALUE
}