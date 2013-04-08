package euler0040

import euler.EulerSolution

object Euler0034 extends EulerSolution {

  override def expect = 40730
  
  def fact(n: Int, total: Int = 1): Int =
    if (n == 0 || n == 1) total else fact(n - 1, total * n)

  def solve = {
    val factorials = Map() ++ (0 to 9).map { x => x -> fact(x) }
    val matches = (3 to fact(9) * 7) filter { x =>
      val sumFacts = x.toString.map({ c => factorials(c.asDigit) }).foldLeft(0) { (sum: Int, next: Int) => sum + next }
      sumFacts - x == 0
    }
    matches.sum
  }

}
