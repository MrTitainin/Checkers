package main

import java.awt.image.BufferedImage
import java.nio.file.Paths
import javax.imageio.ImageIO
import scala.swing.Point

object Lib {
  def getImage(name: String):BufferedImage = {
    val file = Paths.get("./src/main/resources/res/" + name).normalize().toFile
    if(file.isFile) ImageIO.read(file)
    else ImageIO.read(getClass.getClassLoader.getResourceAsStream("res/"+name))
    /*match {
      case Failure(exception) => Try(Paths.get("./src/main/resources/res/" + name).toFile) match {
        case Failure(exception) =>
          println("./src/main/resources/res/"+name)
          throw exception
        case Success(value) => value
      }
      case Success(value) =>
        println(value.isFile)
        value
    }*/
  }

  def linp(fr: Double, v1: Int, v2: Int) = Math.round((1 - fr) * v1 + fr * v2).toInt

  def isInside(p: Point)(left: Int, right: Int, top: Int, bottom: Int) = p.x > left && p.x < right && p.y > top && p.y < bottom
}
