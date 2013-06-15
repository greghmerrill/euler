package euler0060

import euler.EulerSolution
import euler.Primes.primes
import scala.math.abs

object Euler0049 extends EulerSolution {

  type Digits = Map[Char, String]

  override def expect = 296962999629L
  
  def solve: Long = {
    val exclude = Set[Long](1487, 4817, 8147)
    val candidates = primes.dropWhile(_ < 1000).takeWhile(_ < 10000).filter(!exclude.contains(_)).toList

    val digits = candidates map { p => p.toString.groupBy(c => c) }
    val digitsByPrime: Map[Long, Digits] = (candidates zip digits).toMap

    val primesByDigits = digitsByPrime.foldLeft(Map[Digits, List[Long]]().withDefaultValue(List[Long]())) { (result, next) =>
      result + (next._2 -> (next._1 :: result(next._2)).sorted)
    }

    val threePerms = primesByDigits foreach { pair =>
      val combos = pair._2.combinations(3)
      for (combo <- combos) {
        if (abs(combo(2) - combo(1)) == abs(combo(1) - combo(0))) return combo.sorted.foldLeft("") { (str, p) => str + p }.toLong
      }
    }
    throw new IllegalStateException("Solution not found")
  }

}
