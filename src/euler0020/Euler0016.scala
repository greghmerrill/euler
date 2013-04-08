package euler0020

import euler.EulerSolution

object Euler0016 extends EulerSolution {

  override def expect = 1366
  
  def solve = BigInt(2).pow(1000).toString.map(c => ("" + c).toInt).sum

}
