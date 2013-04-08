package euler0020

import euler.EulerSolution
import scala.math._

object Euler0015 extends EulerSolution {

  override def expect = 137846528820L

  def factorial(n: BigInt) = {
    def _factorial(n: BigInt, accum: BigInt): BigInt = if (n == 1) accum else _factorial(n - 1, accum * n)
    _factorial(n, 1)
  }

  // "n choose k" to find a cell in Pascal's Triangle
  def solve = factorial(40) / (factorial(20) * factorial(40 - 20))

}
