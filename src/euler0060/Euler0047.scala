package euler0060

import euler.EulerSolution
import scala.math.sqrt
import euler.Primes.primes

object Euler0047 extends EulerSolution {

  override def expect = 134043
  
  def addFactor(n: Long, factors: Map[Long, Int]): Map[Long, Int] =
    if (factors.contains(n)) factors + (n -> (factors(n) + 1))
    else factors + (n -> 1)

  def areDistinct(factors: List[Map[Long, Int]], reqdSize: Int): Boolean = {
    if (factors.find { m => m.size != reqdSize }.isDefined ) false
    else {
	    val count = factors.foldLeft(0) { (count, map) => count + map.size }
	    val factorLists: List[(Long, Int)] = factors.map(_.toList).flatten
	    factorLists.toSet.size == count
    }
  }

  def primeFactors(n: Long, factors: Map[Long, Int] = Map[Long, Int]()): Map[Long, Int] = {
    val startWith = if (factors.isEmpty) 2 else factors.keys.max
    val candidates = primes.dropWhile(_ < startWith).takeWhile(_ <= n)
    for (p <- candidates) {
      if (n % p == 0) return primeFactors(n / p, addFactor(p, factors))
    }
    factors
  }

  def solve = {
    val reqdConsec = 4
    val upperBound = 1000000
    val numFactors = new Array[Int](upperBound)

    for (i <- primes takeWhile(_ < sqrt(upperBound)) map(_.toInt))
      for (j <- i.to(upperBound - 1, i))
      	numFactors(j) = numFactors(j) + 1

    val targetFactorCounts = (1 to reqdConsec).map({i => reqdConsec})
    val firstMatch = (0.to(upperBound - reqdConsec)).find{ i =>
      val seq = i to (i + reqdConsec - 1)
      val factorCounts = seq.map(numFactors(_))
      if (factorCounts != targetFactorCounts) false
      else {
        val pFacs = seq map { n => primeFactors(n) }
        areDistinct(pFacs.toList, reqdConsec)
      }
    }
    firstMatch.get
  }

}
