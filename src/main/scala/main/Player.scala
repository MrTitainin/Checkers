package main

sealed trait Player{
  def ai = this.asInstanceOf[AIAlgorithm]
  def isHuman = this == Human
  def isAI = this != Human
}
trait AIAlgorithm extends Player {
  def makeMove(s:State):State
  def gameEnded(win:Boolean,moves:Int)
  def avgWinMoves:Int
  def avgMoveTime:Long
  def winRate:Double
}
case object Human extends Player