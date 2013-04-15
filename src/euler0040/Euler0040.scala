package euler0040

import euler.EulerSolution
import scala.math.pow

object Euler0040 extends EulerSolution {

  override def expect = 210

  class ChampTier(val power: Int, val index: Int) {}

  def champ(n: Int): Int = {

    def findTier(power: Int = 0, prevStart: Int = 1): ChampTier = {
      val nextStart = prevStart + (pow(10, power).toInt * 9 * (power + 1))

      if (nextStart > n) new ChampTier(power, prevStart)
      else findTier(power + 1, nextStart)
    }

    val tier = findTier()
    val number = pow(10, tier.power).toInt + ((n - tier.index) / (tier.power + 1))
    number.toString.charAt((n - tier.index) % (tier.power + 1)).asDigit
  }

  def solve = (0 to 6).map(pow(10, _).toInt).map(champ).product

}
