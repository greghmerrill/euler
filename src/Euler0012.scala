import scala.math._

object Euler0012 extends EulerSolution {

  def triangle(n: Int) = ((n * n) + n) / 2

  def calcFactors(value: Int, n: Int, factors: List[Int]): List[Int] = {
    n match {
      case n if n > floor(sqrt(value)).toInt => factors
      case n if value % n == 0 => calcFactors(value, n + 1, n :: value / n :: factors)
      case _ => calcFactors(value, n + 1, factors)
    }
  }
  def factors(value: Int) = calcFactors(value, 1, List())

  def solve = {
    def find(i: Int): Int = {
      val tri = triangle(i)
      if (factors(tri).length > 500) tri else find(i + 1)
    }
    find(2)
  }

}
