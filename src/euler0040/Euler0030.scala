package euler0040

import euler.EulerSolution
import scala.math.pow

object Euler0030 extends EulerSolution {

  override def expect = 443839
  
  def solve = {
    val powers = Map() ++ (0 to 9).map(a => a -> pow(a, 5).toInt)
    val matches = (2 to pow(9, 6).toInt).filter { x =>
      x.toString.map(c => powers(c.asDigit)).sum == x
    }
    matches.sum
  }

}
