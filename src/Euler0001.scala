object Euler0001 extends EulerSolution {
  def solve: Int = {
    def summation(n: Int): Int = { ((n * n) + n) / 2 }
    (3 * summation(999 / 3)) + (5 * summation(999 / 5)) - (15 * summation(999 / 15)) 
  }
}