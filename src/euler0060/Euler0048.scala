package euler0060

import euler.EulerSolution
import scala.math.BigInt

object Euler0048 extends EulerSolution {

  override def expect = 9110846700L
  
  def solve = {
    val series = (1 to 1000) map { i => BigInt(i).pow(i) }
    val sumStr = (series.foldLeft(BigInt(0)) { (next, sum) => sum + next }).toString
    sumStr.substring(sumStr.size - 10, sumStr.length).toLong
  }

}
