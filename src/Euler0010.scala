import euler.EulerSolution
import java.util.BitSet
import scala.math._

object Euler0010 extends EulerSolution {

  override def expect = 142913828922L
  
  def solve = {
    val below = 2000000
    
    val nonPrimes = new BitSet()
    nonPrimes.set(0)
    nonPrimes.set(1)
    
    var primes = List[Int]()
    var prime = nonPrimes.nextClearBit(2)
    
    while (prime < sqrt(below).toInt + 1) {
      primes = prime :: primes
      var nonPrime = prime * 2
      while (nonPrime < below) {
        nonPrimes.set(nonPrime)
        nonPrime += prime
      }
      prime = nonPrimes.nextClearBit(prime + 1)
    }
    
    while (prime < below) {
      primes = prime :: primes
      prime = nonPrimes.nextClearBit(prime + 1)
    }

    primes.foldLeft(0L){(sum, next) => sum + next}
  }

}

