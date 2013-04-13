package euler0040

import euler.EulerSolution

// TODO revisit this one, it's not very elegant
object Euler0033 extends EulerSolution {

  override def expect = 100

  def digitize(x: Int): List[Int] = x.toString.map(_.asDigit).toList
  def toInt(x: List[Int]): Int = x.mkString.toInt

  class Fraction(val num: Int, val denom: Int) {
    def cancel(): Option[Fraction] = {
      val numDigits = digitize(num).filter(!digitize(denom).contains(_))
      val denomDigits = digitize(denom).filter(!digitize(num).contains(_))

      if (numDigits.size != 1 || denomDigits.size != 1) None
      else if (1.0 * toInt(numDigits) / toInt(denomDigits) == 1.0 * num / denom)
        Option(new Fraction(toInt(numDigits), toInt(denomDigits)))
      else None
    }
  }

  def canceledFracs(num: Int): List[Fraction] = {
    val denominators = num to 99 filter (_ % 10 > 0)
    val fracs = denominators.map(denom => new Fraction(num, denom))

    fracs.foldLeft(List[Fraction]()) { (list, frac) =>
      val cancelled = frac.cancel()
      if (cancelled.isDefined) cancelled.get :: list
      else list
    }
  }

  def solve = {
    val numerators = 11 to 99 filter (_ % 10 > 0)
    val cancelled = (numerators map { num => canceledFracs(num) } filter(_.size > 0)).flatten
    val rawProduct = cancelled.foldLeft(new Fraction(1, 1)) { (product, next) =>
      new Fraction(product.num * next.num, product.denom * next.denom)
    }

    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    rawProduct.denom / gcd(rawProduct.num, rawProduct.denom)
  }

}
