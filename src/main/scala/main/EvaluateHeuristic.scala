package main

class EvaluateHeuristic(val alg:State=>Int,val name:String){
  def apply(s:State) = alg(s)
  override def toString: String = name
}
object EvaluateHeuristic {
  val simple = EvaluateHeuristic(simpleValue,"simple value")
  val advanced = EvaluateHeuristic(advancedPreference,"advanced preference")
  val center = EvaluateHeuristic(centerPreference,"center preference")

  def apply(alg:State=>Int,name:String) = new EvaluateHeuristic(alg,name)

  private def centerValue(c:Checker):Int = if(c.isQueen) 10 else {
    val x = State.x(c.i)
    if(x>=4) 7-x else x
  }
  private def advancedValue(c:Checker):Int =
    if(c.isQueen){
      if(c.isWhite) 15 else -15
    } else {
      val y = State.y(c.i)
      if(c.isWhite) y else 7-y
    }

  private def simpleValue(s:State) = s.whites.size + 4 * s.whites.count(_.isQueen) - s.blacks.size - 4 * s.blacks.count(_.isQueen)
  private def centerPreference(s:State):Int = s.checkers.map(c => if (c.isWhite) centerValue(c) else 0 - centerValue(c)).sum
  private def advancedPreference(s:State):Int = s.checkers.map(advancedValue).sum
}
