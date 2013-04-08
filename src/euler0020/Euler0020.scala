package euler0020

import euler.EulerSolution

object Euler0020 extends EulerSolution {

  override def expect = 648

  def factorial(n: BigInt) = {
    def _factorial(n: BigInt, accum: BigInt): BigInt = if (n == 1) accum else _factorial(n - 1, accum * n)
    _factorial(n, 1)
  }

  def solve = factorial(100).toString.map(c => ("" + c).toInt).sum

}
