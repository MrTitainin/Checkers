package main

import main.Game._

import java.awt.Color
import scala.swing._

class UI(p1:Player, p2:Player) extends MainFrame {
  title = "Checkers"
  val canvas = new Canvas(p1=p1,p2=p2)

  val buttonLine = new BoxPanel(Orientation.Horizontal) {
    contents += UI.FancyButton("New Game") { newGame() }
    /*contents += Swing.HGlue
    contents += main.Canvas.timer*/
    contents += Swing.HGlue
    contents += UI.FancyButton("Quit") { sys.exit(0) }
    background = Color.GRAY
  }

  val mainPanel = new BoxPanel(Orientation.Vertical) {
    contents += canvas
    contents += Swing.VStrut(10)
    contents += buttonLine
    border = Swing.EmptyBorder(10, 10, 10, 10)
    background = Color.GRAY
  }

  contents = mainPanel

  def newGame(): Unit = {
    canvas.moves = 0
    canvas.state = State.startingState.copy()
    repaint()
  }
}
object UI{
  def FancyButton(name:String)(action: => Unit) = {
    val b = Button(name)(action)
    b.opaque = true
    b.background = gold
    b
  }

  val gold = Color.decode("0xdb9d00")
  val beige = Color.decode("0xfaebd7")
  val brown = Color.decode("0x4c3627")

  val padding = 10
  val tileSize = 40

  val baseWidth = tileSize*boardLength
  val baseHeight = tileSize*boardLength

  val componentWidth = baseWidth+2*padding
  val componentHeight = baseHeight+2*padding

  val top = padding
  val bottom = padding+baseHeight
  val left = padding
  val right = padding+baseWidth

  val font = new Font("Perpetua",0,40)
}
