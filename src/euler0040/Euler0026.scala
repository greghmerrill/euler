package euler0040

import euler.EulerSolution

object Euler0026 extends EulerSolution {

  override def expect = 983

  def reciprocalCycle(n: Int): Option[List[Int]] = {

    def longDiv(
      dividend: Int,
      divisor: Int,
      quotientDigits: List[Int] = List(),
      testedPairs: Set[(Int, Int)] = Set()): Option[List[Int]] = {

      if (dividend < divisor) longDiv(dividend * 10, divisor, 0 :: quotientDigits, testedPairs)
      else {
        val pair = dividend -> divisor
        val quotient = dividend / divisor
        val remainder = dividend % divisor

        if (remainder == 0) None
        else if (testedPairs.contains(pair)) Option(quotientDigits.reverse)
        else longDiv(remainder * 10, divisor, quotient :: quotientDigits, testedPairs + pair)
      }

    }

    longDiv(10, n)
  }

  def solve = {
    (2 to 999).toList.foldLeft(0 -> 0) { (digitsAndDivisor, n) =>
      val digits = reciprocalCycle(n).getOrElse(List()).length
      if (digits > digitsAndDivisor._1) (digits, n) else digitsAndDivisor
    }._2
  }

}
