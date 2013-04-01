import scala.math.sqrt

object Euler0027 extends EulerSolution {

  override def expect = -59231

  def isPrime(n: Int) = n > 1 && !(2 to sqrt(n).toInt).toList.exists(factor => n % factor == 0)

  def quad(n: Int, a: Int, b: Int): Int = (n * n) + (a * n) + b
  def countPrimes(n: Int, a: Int, b: Int, count: Int = 0): Int = {
    if (!isPrime(quad(n, a, b))) count
    else countPrimes(n + 1, a, b, count + 1)
  }

  class Result(val a: Int, val b: Int, val consecutivePrimes: Int) {
    override def toString(): String = a + " " + b + " " + consecutivePrimes
  }

  def solve = {

    val aRange = (-999 to 999)
    val bRange = (2 to 999).filter(isPrime(_))

    val sortFn = (result: Result) => -result.consecutivePrimes

    val resultsPerA = aRange.map(a => {
      bRange.map(b => new Result(a, b, countPrimes(0, a, b))).sortBy(sortFn).head
    })

    val result = resultsPerA.sortBy(sortFn).head
    result.a * result.b
  }

}
