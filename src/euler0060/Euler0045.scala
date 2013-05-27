package euler0060

import euler.EulerSolution
import euler.QuadraticEquation.quadr
import scala.math.abs

object Euler0045 extends EulerSolution {

  override def expect = 1533776805

  def isPosLong(n: Double) = n > 0 && abs(n - n.round) < .0000001
  def isPentagonal(n: Long): Boolean = {
    val (q1, q2) = quadr(3, -1, -2 * n)
    isPosLong(q1) || isPosLong(q2)
  }

  def hexagonals(n: Long): Stream[Long] = (n * (2 * n - 1)) #:: hexagonals(n + 1)

  def solve = hexagonals(144).find(isPentagonal(_)).head

}
