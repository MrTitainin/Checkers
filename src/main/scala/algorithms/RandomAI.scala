package algorithms

import main.{AIAlgorithm, State}

import scala.util.Random

class RandomAI extends AIAlgorithm {
  override def makeMove(s: State): State = Random.shuffle(s.moves).head.result
}
