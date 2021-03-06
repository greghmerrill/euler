package euler0020

import euler.EulerSolution

object EulerSolution0009 extends EulerSolution {

  override def expect = 31875000
  
  def solve: Int = {
    for (a <- 1 to 332) {
      for (b <- (a + 1) to ((1000 - a) / 2) - 1) {
        val c = 1000 - a - b
        if ((a * a) + (b * b) == c * c) {
          return a * b * c
        }
      }
    }
    throw new Exception("Solution not found")
  }

}
