import euler.EulerSolution

object Euler0004 extends EulerSolution {

  override def expect = 906609

  def solve = {
    def isPalindrome(x: Int) = x.toString.equals(x.toString.reverse)
    val combos = (100 to 999) map { x => (x to 999) map { y => x * y } } flatten;
    combos filter (x => isPalindrome(x)) max
  }

}
