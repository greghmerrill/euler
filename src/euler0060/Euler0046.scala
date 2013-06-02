package euler0060

import euler.EulerSolution
import euler.Primes.isPrime
import scala.annotation.tailrec
import scala.collection.mutable.Map
import euler.Primes.primes

object Euler0046 extends EulerSolution {

  override def expect = 5777
  
  def twiceSquares(n: Long = 1): Stream[Long] = (2 * n * n) #:: twiceSquares(n + 1)

  def solve = {
    @tailrec
    def falseGoldbach(n: Long): Long = {
      if (isPrime(n)) falseGoldbach(n + 2)
      else {
        val possiblePrimes = primes.takeWhile(_ <= n - 2).reverse
        val twiceSqs = twiceSquares().takeWhile(_ <= n - 2)
        val matched = possiblePrimes.find { p => twiceSqs.find { tsq => p + tsq == n }.isDefined }

        if (!matched.isDefined) n
        else falseGoldbach(n + 2)
      }
    }
    falseGoldbach(9)
  }

}
