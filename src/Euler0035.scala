import euler.EulerSolution
import euler.Primes

object Euler0035 extends EulerSolution {

  override def expect = 55

  def isCircularPrime(primeStr: String, primes: Set[Int], rot: Int = 0): Boolean = {
    if (primeStr.size == rot + 1) true
    else {
      val next = primeStr.slice(1, primeStr.length) + primeStr.slice(0, 1)
      primes.contains(next.toInt) && isCircularPrime(next, primes, rot + 1)
    }
  }

  def solve = {
    val primes = Set() ++ Primes.sieve(1000000)
    primes.filter { prime => isCircularPrime(prime.toString, primes) } size
  }

}
