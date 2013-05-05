package euler

import java.util.BitSet
import scala.math.sqrt

object Primes {

  def sieve(below: Int) = {
    val nonPrimes = new BitSet()
    nonPrimes.set(0)
    nonPrimes.set(1)

    val upto = sqrt(below).toInt + 1
    def sieveIter(prime: Int, primes: List[Int] = List()): List[Int] = {
      if (prime > below) primes
      else if (prime > upto) sieveIter(nonPrimes.nextClearBit(prime + 1), prime :: primes)
      else {
        ((prime * 2).to(below, prime)) foreach { nonPrimes.set(_) }
        sieveIter(nonPrimes.nextClearBit(prime + 1), prime :: primes)
      }
    }
    sieveIter(2).reverse
  }

  def isPrime(n: Long): Boolean = {
    
    val sqrtn = sqrt(n).toInt
    
    def primeCheck(factor: Int): Boolean = {
      if (factor > sqrtn) true
      else if (n % factor == 0) false
      else primeCheck(factor + 1)
    }
    
    primeCheck(2)
  }
  
}
