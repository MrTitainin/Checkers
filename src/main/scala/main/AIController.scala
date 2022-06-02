package main

import algorithms.RandomAI

case class AIController(a1: AIAlgorithm, a2:AIAlgorithm, randomFirstMove:Boolean=true){
  var state:State = State.startingState.copy()
  var moves = 0;

  if(randomFirstMove){
    val rai = new RandomAI
    aiMove(rai)
    aiMove(rai)
  }

  def aiMove(ai: AIAlgorithm):Unit = {
    if(!state.active) return
    state = ai.makeMove(state)
    moves+=1
  }


  while(state.active){
    if(state.whiteMove) aiMove(a1)
    else aiMove(a2)
  }

  if(state.lost) {
    if(state.whiteMove) {
      a1.gameEnded(false,moves)
      a2.gameEnded(true,moves)
    }
    else {
      a1.gameEnded(true,moves)
      a2.gameEnded(false,moves)
    }
  }
  else {
    a1.gameEnded(false,moves)
    a2.gameEnded(false,moves)
  }
}
