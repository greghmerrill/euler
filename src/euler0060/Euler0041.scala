package euler0060

import euler.Primes
import euler.EulerSolution

object Euler0041 extends EulerSolution {

  override def expect = 7652413

  def solve = {

    def maxPrimePandigital(startWith: List[Int], digits: List[Int]): Option[Long] = {
      if (startWith.length == digits.length) {
        val pandigital = startWith.reverse.mkString("").toLong
        if (Primes.isPrime(pandigital)) Some(pandigital)
        else None
      } else {
        val remaining = digits filter { d => !(startWith.contains(d)) }
        for (d <- remaining) {
          val mpp = maxPrimePandigital(d :: startWith, digits)
          if (mpp.isDefined) return mpp
        }
        None
      }
    }

    def maxPrimePandigForAllDigSets: Long = {
      // Sum of 9 digits = 45, divisible by 3 so non prime
      // Sum of 8 digits = 36, divisible by 3 so non prime
      for (digCount <- 7.to(1, -1)) {
        val matched = maxPrimePandigital(Nil, digCount.to(1, -1).toList)
        if (matched.isDefined) return matched.get
      }
      throw new IllegalStateException();
    }

    maxPrimePandigForAllDigSets
  }

}
