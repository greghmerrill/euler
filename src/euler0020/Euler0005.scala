package euler0020

import euler.EulerSolution
import scala.math._

object Euler0005 extends EulerSolution {

  override def expect = 232792560

  def calcPrimeFactors(remaining: Int, curFactor: Int, factors: List[Int]): List[Int] = remaining match {
    case 1 => factors
    case n if n % curFactor == 0 => calcPrimeFactors(remaining / curFactor, curFactor, curFactor :: factors)
    case _ => calcPrimeFactors(remaining, curFactor + 1, factors)
  }
  def primeFactors(num: Int) = calcPrimeFactors(num, 2, List[Int]())

  def solve = {
    var allPrimeCounts = Map[Int, Int]()
    2 to 20 foreach { x =>
      val primeCounts = primeFactors(x).foldLeft(Map[Int, Int]()) {
        (counts: Map[Int, Int], prime: Int) => counts + (prime -> (counts.getOrElse(prime, 0) + 1))
      }

      primeCounts.keys.foreach { prime =>
        allPrimeCounts += prime -> max(primeCounts.getOrElse(prime, 0), allPrimeCounts.getOrElse(prime, 0))
      }
    }

    allPrimeCounts.foldLeft(1) {
      (product, primeCount) => product * pow(primeCount._1.toDouble, primeCount._2.toDouble).toInt
    }
  }

}
