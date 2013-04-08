package euler0020

import euler.EulerSolution

object Euler0007 extends EulerSolution {

  override def expect = 104743

  def solve = {
    def nthPrime(nextVal: Int, primes: List[Int], n: Int): Int = {
      if (primes.length == n) primes.head
      else if (primes.reverse.exists(nextVal % _ == 0)) nthPrime(nextVal + 2, primes, n)
      else nthPrime(nextVal + 2, nextVal :: primes, n)
    }
    nthPrime(3, List(2), 10001)
  }

}
