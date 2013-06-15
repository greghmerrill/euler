package euler0060

import scala.annotation.tailrec

import euler.EulerSolution
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
    @tailrec
    def find(n: Int): Int = {
      if (n % 100 == 0) println(n)
      val factors = primeFactors(n)
      if (factors.size != reqdConsec) find(n + reqdConsec)
      else {
	      val nFactors = factors :: ((1 to reqdConsec - 1) map { i => primeFactors(n - i) }).toList
	      if (areDistinct(nFactors.toList, reqdConsec)) n - reqdConsec + 1 
	      else find(n + 1)
      }
    }
    find(reqdConsec + 1)
  }

}
