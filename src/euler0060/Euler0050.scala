package euler0060

import euler.EulerSolution
import euler.Primes.primes
import euler.Primes.sieve

object Euler0050 extends EulerSolution {

  override def expect = 997651

  def solve = {
    val upTo = 1000000
    val primes = sieve(upTo)
    val primeSet = primes.toSet

    def largestPrimeSeq(remPrimes: List[Int], curSum: Int = 0, curSeries: Set[Int] = Set(), curMaxSeries: Set[Int] = Set()): Set[Int] = remPrimes match {
      case Nil => curMaxSeries
      case default => {
        val newSum = remPrimes.head + curSum
        if (newSum > upTo) curMaxSeries
        else {
          val newSeries = curSeries + remPrimes.head
          val newMaxSeries = if (primeSet.contains(newSum)) newSeries else curMaxSeries
          largestPrimeSeq(remPrimes.tail, newSum, newSeries, newMaxSeries)
        }
      }
    }

    def processPrimes(primes: List[Int], maxPrimeSeries: Set[Int] = Set()): Set[Int] = primes match {
      case Nil => maxPrimeSeries
      case default => {
        val curIterLargestSet = largestPrimeSeq(primes)
        val newBiggestSeries = if (curIterLargestSet.size > maxPrimeSeries.size) curIterLargestSet else maxPrimeSeries
        processPrimes(primes.tail, newBiggestSeries)
      }
    }

    processPrimes(primes).sum
  }

}
