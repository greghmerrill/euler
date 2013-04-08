package euler0040

import euler.EulerSolution
import scala.math._

object Euler0021 extends EulerSolution {

  override def expect = 31626
  
  def divisors(n: Int) = (1 to n / 2).filter { next => n % next == 0 }

  def solve = {
    val sumOfDivisors = (1 to 10000).map { n => (n -> divisors(n).sum) }.toMap
    val amicables = sumOfDivisors.filterKeys { k =>
      k != sumOfDivisors(k) && k == sumOfDivisors.getOrElse(sumOfDivisors(k), -1)
    }
    amicables.keys.sum
  }

}
