import scala.math._

object Euler0005 extends EulerSolution {

  override def expect = 232792560
  
  def solve = {
    val primes = List(2, 3, 5, 7, 11, 13, 17, 19)
    var allPrimeCount = Map[Int, Int]()
    2 to 20 foreach { x =>
      var remaining = x
      var primeFactors = List[Int]()
      var remainingPrimes = primes
      while (remaining > 1) {
        if (remaining % remainingPrimes.head == 0) {
          primeFactors = remainingPrimes.head :: primeFactors
          remaining = remaining / remainingPrimes.head
        } else {
          remainingPrimes = remainingPrimes.tail
        }
      }

      var primeCounts = Map[Int, Int]()
      primeFactors foreach { prime =>
        primeCounts += prime -> (primeCounts.getOrElse(prime, 0) + 1)
      }
      primeCounts.keys.foreach { prime =>
        allPrimeCount += prime -> max(primeCounts.getOrElse(prime, 0), allPrimeCount.getOrElse(prime, 0))
      }
    }
    allPrimeCount.foldLeft(1)({ (product, primeCount) => product * pow(primeCount._1.toDouble, primeCount._2.toDouble).toInt })
  }

}
