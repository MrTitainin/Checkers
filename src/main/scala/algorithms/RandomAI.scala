package algorithms

import main.{AIAlgorithm, State}

import scala.util.Random

class RandomAI extends AIAlgorithm {
  override def makeMove(s: State): State = Random.shuffle(s.moves).head.result

  override def avgWinMoves: Int = ???

  override def avgMoveTime: Long = ???

  override def winRate: Double = ???

  override def gameEnded(win: Boolean, moves: Int): Unit = ???
}
