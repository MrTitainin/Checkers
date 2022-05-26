package main

import scala.swing.Point

object Lib {
  def linp(fr: Double, v1: Int, v2: Int) = Math.round((1 - fr) * v1 + fr * v2).toInt

  def isInside(p: Point)(left: Int, right: Int, top: Int, bottom: Int) = p.x > left && p.x < right && p.y > top && p.y < bottom
}
