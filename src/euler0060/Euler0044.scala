package euler0060

import euler.EulerSolution
import scala.math.abs
import scala.math.sqrt
import euler.QuadraticEquation.quadr

object Euler0044 extends EulerSolution {

  override def expect = 5482660
  
  def pentagonal(n: Int) = n * (3 * n - 1) / 2
  def pentagonals(n: Int): Stream[Int] = pentagonal(n) #:: pentagonals(n + 1)
  def isPentagonal(n: Int) = {
    val (q1, q2) = quadr(3, -1, -2 * n)
    val close = abs(q1 - q1.round) < .001 || abs(q2 - q2.round) < .001
    close && pentagonals(1).takeWhile(_ <= n).contains(n)
  }
  
  class PentPair(val p1: Int, val p2: Int) {
    val diff = p1 - p2
    val sum = p1 + p2
    override def toString = s"(${p1},${p2}=${p1-p2})"
  }

  def pentPairs(n: Int): List[PentPair] = {
    val larger = pentagonal(n)
    val stream = 
      for (smaller <- pentagonals(1).takeWhile(_ < larger) if isPentagonal(larger - smaller) && isPentagonal(larger + smaller)) 
      yield new PentPair(larger, smaller)
    val list = stream.toList
    
    // It's possible that a subsequent pair *could* have a smaller diff, although it wasn't an issue for this particular question
    if (list.size > 0) list
    else pentPairs(n + 1)
  }
  
  def solve = {
    val pairs = pentPairs(1).sortBy { p => p.diff }
    pairs.filter(p => isPentagonal(p.diff) && isPentagonal(p.sum)).head.diff
  }

}
