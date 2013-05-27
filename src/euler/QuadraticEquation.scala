package euler

import scala.math.sqrt

object QuadraticEquation {

  def quadr(a: Long, b: Long, c: Long): (Double, Double) = {
    val sqrt_bsq4ac = sqrt((b * b) - (4 * a * c))
    (((-b + sqrt_bsq4ac) / (2 * a)), ((-b - sqrt_bsq4ac) / (2 * a)))
  }

}