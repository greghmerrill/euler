package euler0020

import euler.EulerSolution

object Euler0003 extends EulerSolution {

  override def expect = 6857

  def solve: Long = {
    def largestPrimeFactor(remaining:Long, factor:Long): Long = remaining match {
      case 1 => factor
      case n if (n % factor == 0) => largestPrimeFactor(n / factor, factor)
      case _ => largestPrimeFactor(remaining, factor + 1)
    } 
    largestPrimeFactor(600851475143L, 2)
  }

}
