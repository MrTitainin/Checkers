package main

import main.Checker._

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.swing.{Graphics2D, Point}

case class Checker(i:Int, isQueen:Boolean, colour:CheckerColour) {
  var pos:Point = State.tileStart(i)

  def draw(g:Graphics2D): Unit = g.drawImage(img,pos.x,pos.y,null)
  def drawHint(g:Graphics2D): Unit = g.drawImage(Move.moveMark,pos.x,pos.y,null)

  def isWhite = colour == WHITE
  def isBlack = colour == BLACK

  def img = colour match{
    case WHITE => if(isQueen) whiteQueen else whitePawn
    case BLACK => if(isQueen) blackQueen else blackPawn
  }

  def promotionLine = if(isWhite) promotionWhite else promotionBlack
  def isPromotable = !isQueen && State.y(i) == promotionLine

  def move(n:Int) = Checker(n, isQueen, colour)
  def promote() = Checker(i, true, colour)

  implicit def some = Some(this)
}
object Checker{
  val whitePawn:BufferedImage = ImageIO.read(new File("res/whitePawn.png"))
  val whiteQueen:BufferedImage = ImageIO.read(new File("res/whiteQueen.png"))
  val blackPawn:BufferedImage = ImageIO.read(new File("res/blackPawn.png"))
  val blackQueen:BufferedImage = ImageIO.read(new File("res/blackQueen.png"))

  val promotionWhite = 0
  val promotionBlack = Game.boardLength-1

  sealed trait CheckerColour
  case object WHITE extends CheckerColour
  case object BLACK extends CheckerColour
}
