package euler0040

import euler.EulerSolution

object Euler0032 extends EulerSolution {

  override def expect = 45228

	object Digits {
	  val list = (1 to 9).toList
	  val set = Set() ++ list
	}
  
  /** @return all ordered, non-repeating combinations with "size" elements from the given vals */
  def combos(vals: List[Int], size: Int): List[List[Int]] = {
    def comboIter(combo: List[Int], remainingVals: List[Int]): List[List[Int]] = {
      if (combo.size == size) List(combo)
      else {
        remainingVals.map(v => comboIter(v +: combo, remainingVals.filterNot(_ == v))).flatten
      }
    }
    comboIter(List(), vals)
  }

  def toInt(digits: List[Int]): Int = digits.mkString.toInt

  def isPanDigital(a: List[Int], b: List[Int], p: Int) = {
    val str = toInt(a).toString + toInt(b).toString + p.toString
    str.length == 9 && Set() ++ str.map(_.asDigit) == Digits.set
  }

  def panDigitalProducts(multiplicand: List[Int], multipliers: List[List[Int]]): List[Int] = {
    val products = multipliers map { x => (x, toInt(multiplicand) * toInt(x)) }
    val panDigitalsOnly = products.filter { mz =>
      isPanDigital(multiplicand, mz._1, mz._2)
    }
    panDigitalsOnly.map { mz => mz._2 }
  }

  def panDigitalProductsForMultiplicand(multiplicand: List[Int]): List[Int] = {
    val multipliers = combos(Digits.list.filterNot(multiplicand.contains(_)), 5 - multiplicand.size)
    panDigitalProducts(multiplicand, multipliers)
  }

  def solve = {
    val pandigitalProducts = (1 to 2) map { size =>
      combos(Digits.list, size) map panDigitalProductsForMultiplicand
    }
    (Set() ++ pandigitalProducts.flatten).flatten.sum
  }

}
