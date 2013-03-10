import scala.math._

object Euler0012 extends EulerSolution {

  override def expect = 76576500
  
  def triangle(n: Int) = ((n * n) + n) / 2

  def calcPrimeFactors(remaining: Int, curFactor: Int, factors: List[Int]): List[Int] = remaining match {
    case 1 => factors
    case n if n % curFactor == 0 => calcPrimeFactors(remaining / curFactor, curFactor, curFactor :: factors)
    case _ => calcPrimeFactors(remaining, curFactor + 1, factors)
  }
  def primeFactors(num: Int) = calcPrimeFactors(num, 2, List[Int]())

  // # divisors for n = product of (count of each prime factor + 1)
  def factors(n: Int):Int = primeFactors(n).groupBy(n => n).map(keyVal => keyVal._2.length + 1).product

  def solve = {
    def find(i: Int): Int = {
      val tri = triangle(i)
      if (factors(tri) > 500) tri else find(i + 1)
    }
    find(2)
  }

}
