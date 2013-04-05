object Euler0028 extends EulerSolution {

  override def expect = 669171001
  
  def sumDiags(width: Int, sum: Int): Int =
    if (width == 1) sum + 1
    else sumDiags(width - 2, sum + (width * width * 4) - ((width - 1) * 6))

  def solve = sumDiags(1001, 0)

}
