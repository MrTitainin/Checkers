package main

import algorithms.{AlphaBeta, MinMax, RandomAI}

import java.io.FileWriter
import java.nio.file.Paths

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

  def pit(a1: AIAlgorithm,a2:AIAlgorithm) =
    for(_ <- 1 to 10)
      AIController(a1,a2)

  //new Game()
  //new Game(p2 = new MinMax(8))
  //new Game(p2 = new RandomAI)
  //new Game(p2 = Human)
  //new Game(p2 = new AlphaBeta(10))
  //new Game(new MinMax(6),new RandomAI,true)
  //new Game(new AlphaBeta(10),new MinMax(5),true,true)

  /*val mm3 = new MinMax(3)
  val mm5 = new MinMax(5)
  //val mm8 = new MinMax(8)
  val ab3 = new AlphaBeta(3)
  val ab5 = new AlphaBeta(5)
  val ab8 = new AlphaBeta(8)
  val ab10 = new AlphaBeta(10)
  //val ab12 = new AlphaBeta(12)
  val rai = new RandomAI

  val minmaxes = List(mm3,mm5)
  val alphabetas = List(ab3,ab5,ab8)
  val all = minmaxes:::alphabetas*/

  /*for(alg<-all){
    pit(alg,rai)
    println(alg.toString)
  }

  println("vs rai completed")*/

  /*for(a1<-all) for(a2<-all){
    pit(a1,a2)
    println(a1.toString+a2.toString)
  }

  println("mm vs ab completed")*/

  /*
  val default = new AlphaBeta()
  val adv = new AlphaBeta(evaluate = EvaluateHeuristic.advanced)
  val cnt = new AlphaBeta(evaluate = EvaluateHeuristic.center)

  val heuristics = List(default,adv,cnt)


  for(alg1 <- heuristics) for(alg2 <- heuristics) if(alg1!=alg2) {
    pit(alg1,alg2)
    println(alg1.toString+alg2.toString)
  }

  println("heuristics completed")*/

  /*val fw = new FileWriter(Paths.get("csv/vsAll.csv").toFile)
  fw.write("alg,winrate,winmoves,movetime\n")
  for(alg<-heuristics) fw.write(s"${alg.toString},${alg.winRate},${alg.avgWinMoves},${alg.avgMoveTime}\n")
  fw.close()*/
}
