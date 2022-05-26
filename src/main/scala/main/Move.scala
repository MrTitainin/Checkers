package main

import main.Move._
import main.State.{Board, pos, x, y}
import main.Tile._

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

case class Move(origin:Checker, first:Int, hits:Int, stt: ()=>State) {
  lazy val result = stateCached(stt())
  lazy val drawOrigin = State.tileStart(first)

  def isMove = hits == 0
  def isHit = hits > 0
  def isComplex = hits > 1

  def complexFirstStep(s:State) =
    if(!isComplex) throw new IllegalStateException("First step of non-complex move")
    else (furtherTile(s.board(origin.i),first) match{
      case None => // queen long-distance hit fix
        val diff = (x(first)-x(origin.i),y(first)-y(origin.i))
        val fieldLen = Math.abs(diff._1)
        pos(x(first)+diff._1/fieldLen,y(first)+diff._2/fieldLen)
      case Some(i) => i // normal hit
    }).pipe(i=>s.halfHit(i,hit(origin.i,first,i,s.board)).pipe(ns => (ns,ns.board(i).checker)))

  def draw(g:Graphics2D) = g.drawImage(if(isMove) moveMark else if(isComplex) complexMark else hitMark, drawOrigin.x, drawOrigin.y, null)
}
object Move {
  def apply(c:Checker)(implicit s:State): List[Move] = {

    def generalMove(i: Int, dir:Dir, board:Board)(empty:Int=>List[Move])(hit:(Int,Int)=>List[Move]):List[Move] = {
      board(i).neighbours.get(dir) match { //does any tile exist in that direction
        case Some(target) =>
          board(target).checker match { // does this tile have any piece
            case Some(checker) =>
              if(s.whiteMove != checker.isWhite) // is piece enemy's
                board(target).neighbours.get(dir) match { //does any tile exist in that direction
                  case Some(goal) => board(goal).checker match { // is there any piece on it
                    case Some(_) => List() // blocked by piece
                    case None => hit(target,goal)
                  }
                  case None => List() // board end
                }
              else List() // blocked by own piece
            case None => empty(target) // nothing to hit, move continuation
          }
        case None => List() // board end
      }
    }

    def exploreMove(i:Int, dir:Dir): List[Move] = generalMove(i, dir, s.board)(
        target => List(Move(c,target,0, () => s.moved(i, target, false) )) // normal move
      )( (by,to) => {
        val newBoard = hit(i,by,to,s.board)
        Move(c,by,1,() => s.hit(tryPromotion(to,newBoard))) :: newBoard(to).neighbours.keys.map(exploreHit(to,by,_,newBoard,1)).reduce(_:::_) // hit and all possible follow-up hits
      })

    def exploreQueenMove(i: Int, o:Int, dir: Dir): List[Move] = generalMove(i, dir, s.board)(
        target => List(Move(c,target,0, () => s.moved(o, target, true) )) ::: exploreQueenMove(target, o, dir) // normal move and continuation
      )( (by,to) => {
        val newBoard = hit(o,by,to,s.board)
        Move(c,by,1,() => s.hit(tryPromotion(to,newBoard))) :: newBoard(to).neighbours.keys.map(exploreQueenHit(to,to,by,_,newBoard,1)).reduce(_:::_) // hit and all possible follow-up hits
      })

    def checkHit(i: Int, dir: Dir):List[Move] = s.board(i).neighbours.get(dir) match {
      case Some(value) => exploreHit(i,value,dir,s.board,0)
      case None => List()
    }

    def exploreHit(i: Int, first: Int, dir: Dir, board: Board, count:Int): List[Move] = generalMove(i, dir, board)(
        _ => List()
      )( (by,to) => {
        val newBoard = hit(i,by,to,board)
        Move(c,first,count+1,() => s.hit(tryPromotion(to,newBoard))) :: newBoard(to).neighbours.keys.map(exploreHit(to,first,_,newBoard,count+1)).reduce(_:::_) // hit and all possible follow-up hits
      })

    def exploreQueenHit(i: Int, o:Int, first:Int, dir: Dir, board: Board, count:Int):List[Move] =
      generalMove(i, dir, board)(
        target => exploreQueenHit(target,o,first,dir,board,count) // check further fields
      )( (by,to) => {
        val newBoard = hit(o,by,to,board)
        Move(c,first,count+1,() => s.hit(tryPromotion(to,newBoard))) :: newBoard(to).neighbours.keys.map(exploreQueenHit(to,to,first,_,newBoard,count+1)).reduce(_:::_) // hit and all possible follow-up hits
      })

    if (c.isQueen)
      exploreQueenMove(c.i, c.i, LT) ::: exploreQueenMove(c.i, c.i, RT) ::: exploreQueenMove(c.i, c.i, RB) ::: exploreQueenMove(c.i, c.i, LB)
    else
      if (c.isWhite) exploreMove(c.i, LT) ::: exploreMove(c.i, RT) ::: checkHit(c.i, RB) ::: checkHit(c.i, LB)
      else exploreMove(c.i, LB) ::: exploreMove(c.i, RB) ::: checkHit(c.i, RT) ::: checkHit(c.i, LT)
  }

  val moveMark:BufferedImage = Lib.getImage("move.png")
  val hitMark:BufferedImage = Lib.getImage("hit.png")
  val complexMark:BufferedImage = Lib.getImage("complex.png")

  val statesCache = mutable.WeakHashMap[State,State]()
  def stateCached(s:State) = statesCache.getOrElseUpdate(s,s)

  def hit(from:Int, by:Int, to:Int, board: Board):Board =
    move(from,to,board)
      .updated(by,board(by).unplace()) // remove from 'by'
  def move(from:Int, to:Int, board: Board):Board =
    board.updated(to,board(to).place(board(from).checker.get)) // place on 'to'
      .updated(from,board(from).unplace()) // remove from 'from'
  def hitFinal(from:Int, by:Int, to:Int, board: Board):Board =
    tryPromotion(to,hit(from, by, to, board)) // remove from 'by'
  def moveFinal(from:Int, to:Int, board: Board):Board =
    tryPromotion(to,move(from, to, board)) // remove from 'by'

  def tryPromotion(i:Int, board: Board) = {
    val c = board(i).checker.get
    if(c.isPromotable) board.updated(i,board(i).place(c.promote()))
    else board
  }
}
