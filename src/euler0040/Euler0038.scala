package euler0040

import euler.EulerSolution

object Euler0038 extends EulerSolution {

  object Digits { val all = Set() ++ 1.to(9) }

  def isPanDigital(product: String) = {
    if (product.size != 9) false
    else (Digits.all -- product.map(_.asDigit)).isEmpty
  }

  def concatProducts(n: Int, multiplier: Int = 1, products: String = ""): String = {
    val joined = products + (n * multiplier).toString
    if (joined.size > 8) joined
    else concatProducts(n, multiplier + 1, joined)
  }

  def solve = {
    def hasPanDigitalProducts(n: Int) = isPanDigital(concatProducts(n))
    val max = 1.to(9999).filter(hasPanDigitalProducts).max
    concatProducts(max).toInt
  }

}

