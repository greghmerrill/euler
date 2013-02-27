import scala.math.pow

object Euler0006 extends EulerSolution {

  override def expect = 25164150
  
  def solve = {
    val sumOfSquares = (1 to 100).foldLeft(0) { (sum, next) => sum + next * next }
    val squareOfSums = pow((1 to 100) sum, 2)
    squareOfSums - sumOfSquares
  }

}
