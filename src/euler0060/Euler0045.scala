package euler0060

import euler.EulerSolution
import euler.QuadraticEquation.quadr
import scala.math.abs

object Euler0045 extends EulerSolution {

  override def expect = 1533776805

  def hex(n: Long) = n * (2 * n - 1)
  def hexes(n: Long): Stream[Long] = hex(n) #:: hexes(n + 1)

  def isPosLong(n: Double) = abs(n - n.round) < .0000001 && n > 0
  def isPentagonal(n: Long): Boolean = {
    val (q1, q2) = quadr(3, -1, -2 * n)
    isPosLong(q1) || isPosLong(q2)
  }

  def solve = hexes(144).find { t => isPentagonal(t) }.head

}
