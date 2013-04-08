package euler0040

import euler.EulerSolution

object Euler0029 extends EulerSolution {

  override def expect = 9183

  def solve = {
    val powers = (2 to 100) flatMap { a =>
      (2 to 100) map { b => BigInt(a).pow(b) }
    }
    (Set() ++ powers).size
  }

}
