package main

import main.State.{pos, x, y}
import main.Tile.Dir
import main.UI.{beige, brown, tileSize}

import java.awt.geom.Rectangle2D
import scala.swing.Graphics2D
import scala.util.chaining.scalaUtilChainingOps

case class Tile(num:Int,checker:Option[Checker],img:Rectangle2D,neighbours:Map[Dir,Int]) {
  def place(c:Checker) = Tile(num,c.move(num).some,img,neighbours)
  def unplace() = Tile(num,None,img,neighbours)
  def hasChecker = checker.isDefined

  def draw(g:Graphics2D) = {
    g.setColor(if(isBlack) brown else beige)
    //g.draw(img)
    g.fill(img)
    checker.foreach(_.draw(g))
  }
  def drawNumber(g:Graphics2D) = {
    g.drawString(s"$num",img.getMinX.toInt,img.getMinY.toInt+UI.padding)
    checker.foreach(_.draw(g))
  }

  lazy val isBlack = (State.x(num)+State.y(num))%2 == 1
}
object Tile{
  def apply(num:Int,checker:Option[Checker]) = new Tile(num,checker,State.tileStart(num).pipe(p=>new Rectangle2D.Double(p.x,p.y,tileSize,tileSize)),neighbours(num))
  def from(i:Int,dir:Dir) = (dir.x+x(i),dir.y+y(i)).pipe( c=> if(c._1>=0 && c._1<Game.boardLength && c._2>=0 && c._2<Game.boardLength) Some(pos(c._1,c._2)) else None)
  def neighbours(i:Int) = List(LT->from(i,LT),LB->from(i,LB),RT->from(i,RT),RB->from(i,RB)).filter(_._2.isDefined).map(c=>(c._1.asInstanceOf[Dir],c._2.get)).toMap
  def dir(from:Tile,to:Int) = from.neighbours.find(d => d._2==to).get._1
  def furtherTile(i:Tile,through:Int) = from(through,dir(i,through)).get


  sealed abstract class Dir(val x:Int, val y:Int){
    def dir = (x,y)
  }
  case object LT extends Dir(-1,-1)
  case object LB extends Dir(-1,1)
  case object RT extends Dir(1,-1)
  case object RB extends Dir(1,1)
}
