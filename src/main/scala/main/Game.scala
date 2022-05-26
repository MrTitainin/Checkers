package main

import algorithms.{AlphaBeta, MinMax, RandomAI}

class Game(p1:Player = Human, p2:Player = new MinMax(), auto:Boolean=false, randomFirstMove:Boolean=false){
  val ui = new UI(p1,p2)
  ui.visible = true

  if(randomFirstMove){
    val rai = new RandomAI
    ui.canvas.aiMove(rai)
    ui.canvas.aiMove(rai)
  }

  if(auto && p1.isAI && p2.isAI) new Thread(){
    override def run(): Unit = {
      while(ui.canvas.state.active){
        ui.canvas.makeAiMove()
        Thread.sleep(50)
      }
    }
  }.start()
}
object Game extends App {
  val boardLength = 8
  val boardSize = boardLength*boardLength
  val idleLimit = 15

  def pit(a1: AIAlgorithm,a2:AIAlgorithm) = {

  }
  new Game()
  //new Game(p2 = new RandomAI)
  //new Game(p2 = Human)
  //new Game(p2 = new AlphaBeta(10))
  //new Game(new MinMax(6),new RandomAI,true)
  //new Game(new AlphaBeta(10),new MinMax(5),true,true)
}
