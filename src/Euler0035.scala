import java.util.BitSet
import scala.math.sqrt

object Euler0035 extends EulerSolution {

  override def expect = 55

  def isCircularPrime(primeStr: String, primes: Set[Int], rot: Int = 0): Boolean = {
    if (primeStr.size == rot + 1) true
    else {
      val next = primeStr.slice(1, primeStr.length) + primeStr.slice(0, 1)
      primes.contains(next.toInt) && isCircularPrime(next, primes, rot + 1)
    }
  }

  def primeSieve(below: Int) = {
    val nonPrimes = new BitSet()
    nonPrimes.set(0)
    nonPrimes.set(1)

    val upto = sqrt(below).toInt + 1
    def sieve(prime: Int, primes: List[Int]): List[Int] = {
      if (prime > below) primes
      else if (prime > upto) sieve(nonPrimes.nextClearBit(prime + 1), prime :: primes)
      else {
        ((prime * 2).to(below, prime)) foreach { nonPrimes.set(_) }
        sieve(nonPrimes.nextClearBit(prime + 1), prime :: primes)
      }
    }
    sieve(nonPrimes.nextClearBit(2), List()).reverse
  }

  def solve = {
    val primes = Set() ++ primeSieve(1000000)
    primes.filter { prime => isCircularPrime(prime.toString, primes) } size
  }

}
