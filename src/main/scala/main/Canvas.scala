package main

import algorithms.MinMax
import main.State.Board
import main.UI._

import scala.swing._
import scala.swing.event.{Key, KeyPressed, KeyReleased, MouseClicked}

class Canvas(var state:State = State.startingState.copy(), var p1:Player = Human, var p2:Player = new MinMax()) extends Component {
  var moves = 0
  var selected:Option[Checker] = None
  var vPressed = false
  var nPressed = false

  preferredSize = new Dimension(componentWidth, componentHeight)

  listenTo(mouse.clicks,mouse.moves,keys)
  reactions += {
    case MouseClicked(_, p, _, _, _) => mouseClick(p)
    case KeyPressed(_,k,_,_) => if(k == Key.V) {
      vPressed = true
      repaint()
    }else if(k == Key.N) {
      nPressed = true
      repaint()
    }
    case KeyReleased(_,k,_,_) => if(k == Key.V) {
      vPressed = false
      repaint()
    }else if(k == Key.N) {
      nPressed = false
      repaint()
    }
  }
  focusable = true
  requestFocus()

  def duringHit: Boolean = state.halfState
  def activeMoves: Seq[Move] = state.movesFor(selected)
  def activeCheckers = state.moves.map(_.origin).toSet
  def board: Board = state.board

  private def select(c:Checker): Unit = {
    selected = c.some
    //println(s"selected ${c.i}")
  }
  private def unselect(): Unit = selected = None

  private def isOnBoard(p:Point) = {
    Lib.isInside(p)(left,right,top,bottom)
  }

  def makeAiMove() = aiMove(currentPlayer.ai)

  def currentPlayer = if(state.whiteMove) p1 else p2
  def otherPlayer = if(state.whiteMove) p2 else p1

  def aiMove(ai: AIAlgorithm):Unit = {
    if(!state.active) return
    state = ai.makeMove(state)
    moves+=1
    repaint()
  }

  private def mouseClick(p:Point): Unit = {
    if(isOnBoard(p) && state.active){
      if(currentPlayer.isHuman){
        val tile = board(State.getTile(p))
        tile.checker match {
          case Some(c) if c.isWhite == state.whiteMove => select(c)
          case _ => activeMoves.find(_.first == tile.num) match {
            case Some(move) =>
              if(move.isComplex){
                val ns = move.complexFirstStep(state)
                state = ns._1
                selected = ns._2
              }
              else {
                state = move.result
                moves+=1
                unselect()
                if(currentPlayer.isAI) aiMove(currentPlayer.ai)
              }
            case None => unselect()
          }
        }
      }
      else aiMove(currentPlayer.ai)
      repaint()
    }
  }

  override def paintComponent(g : Graphics2D): Unit = {
    state.draw(g)
    activeMoves.foreach(_.draw(g))
    g.setColor(UI.gold)
    g.setFont(UI.font)
    if(state.isDraw) g.drawString("REMIS", left+padding, top+baseHeight/2)
    if(state.lost) g.drawString(if(state.whiteMove) "PRZEGRANA" else "WYGRANA", left+padding, top+baseHeight/2)
    if(vPressed) activeCheckers.foreach(_.drawHint(g))
    if(nPressed) board.foreach(t=>if(t.isBlack) t.drawNumber(g))
  }

}
object Canvas {

}